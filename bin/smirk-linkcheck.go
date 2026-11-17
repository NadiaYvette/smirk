package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

var (
	linkRegex      = regexp.MustCompile(`!?\[([^\]]*)\]\(([^)\s]+)(?:\s+["'][^"']*["'])?\)`)
	headingRegex   = regexp.MustCompile(`^(#{1,6})\s+(.+)$`)
	htmlTagRegex   = regexp.MustCompile(`<[^>]+>`)
	mdLinkRegex    = regexp.MustCompile(`\[([^\]]*)\]\([^)]+\)`)
	punctRegex     = regexp.MustCompile(`[` + "`" + `*_{}\[\]()#+\-.!?:,'"]`)
	spacesRegex    = regexp.MustCompile(`\s+`)
	externalPrefix = []string{"http://", "https://", "mailto:", "rad:", "ftp://"}
)

func githubSlugify(text string) string {
	text = htmlTagRegex.ReplaceAllString(text, "")
	text = mdLinkRegex.ReplaceAllString(text, "$1")
	text = punctRegex.ReplaceAllString(text, "")
	text = strings.ToLower(strings.TrimSpace(text))
	return spacesRegex.ReplaceAllString(text, "-")
}

func parseAnchors(filePath string) (map[string]bool, int) {
	anchors := make(map[string]bool)
	file, err := os.Open(filePath)
	if err != nil {
		return anchors, 0
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lineCount := 0
	for scanner.Scan() {
		lineCount++
		line := scanner.Text()
		if strings.HasSuffix(filePath, ".md") {
			if m := headingRegex.FindStringSubmatch(line); len(m) > 2 {
				slug := githubSlugify(m[2])
				anchors[slug] = true
			}
		}
	}
	return anchors, lineCount
}

type brokenLink struct {
	line   int
	target string
	err    string
}

func checkLinks(filePath, repoRoot string, anchorCache map[string]map[string]bool) (int, []brokenLink) {
	var broken []brokenLink
	checked := 0

	file, err := os.Open(filePath)
	if err != nil {
		return 0, []brokenLink{{line: 0, target: filePath, err: "Missing file"}}
	}
	defer file.Close()

	reader := bufio.NewReader(file)
	lineNum := 0
	inCode := false

	for {
		line, err := reader.ReadString('\n')
		if err != nil && err != io.EOF {
			break
		}
		lineNum++

		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "```") {
			inCode = !inCode
			if err == io.EOF {
				break
			}
			continue
		}
		if inCode {
			if err == io.EOF {
				break
			}
			continue
		}

		matches := linkRegex.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			if len(m) < 3 {
				continue
			}
			target := m[2]
			isExt := false
			for _, p := range externalPrefix {
				if strings.HasPrefix(target, p) {
					isExt = true
					break
				}
			}
			if isExt {
				continue
			}

			checked++
			var pathPart, anchor string
			if hashIdx := strings.Index(target, "#"); hashIdx >= 0 {
				pathPart = target[:hashIdx]
				anchor = target[hashIdx+1:]
			} else {
				pathPart = target
			}

			var resolved string
			if pathPart != "" {
				resolved = filepath.Clean(filepath.Join(filepath.Dir(filePath), pathPart))
			} else {
				resolved = filePath
			}

			rel, relErr := filepath.Rel(repoRoot, resolved)
			if relErr != nil || strings.HasPrefix(rel, "..") || rel == ".." {
				broken = append(broken, brokenLink{
					line:   lineNum,
					target: target,
					err:    fmt.Sprintf("Link '%s' escapes repository root", resolved),
				})
				continue
			}

			if _, statErr := os.Stat(resolved); os.IsNotExist(statErr) {
				broken = append(broken, brokenLink{
					line:   lineNum,
					target: target,
					err:    fmt.Sprintf("File '%s' does not exist", resolved),
				})
				continue
			}

			if anchor != "" && strings.HasSuffix(resolved, ".md") {
				anchors, ok := anchorCache[resolved]
				if !ok {
					anchors, _ = parseAnchors(resolved)
					anchorCache[resolved] = anchors
				}
				if !anchors[strings.ToLower(anchor)] {
					broken = append(broken, brokenLink{
						line:   lineNum,
						target: target,
						err:    fmt.Sprintf("Anchor '#%s' missing in '%s'", anchor, resolved),
					})
				}
			}
		}

		if err == io.EOF {
			break
		}
	}

	return checked, broken
}

func main() {
	quiet := flag.Bool("quiet", false, "suppress output")
	flag.BoolVar(quiet, "q", false, "suppress output (shorthand)")
	flag.Parse()

	exePath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to get executable path: %v\n", err)
		os.Exit(1)
	}
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(exePath), ".."))

	var targets []string
	_ = filepath.Walk(repoRoot, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if info.IsDir() {
			base := filepath.Base(p)
			if base == ".git" || base == "dist-newstyle" {
				return filepath.SkipDir
			}
			return nil
		}
		if strings.HasSuffix(p, ".md") {
			targets = append(targets, p)
		}
		return nil
	})
	sort.Strings(targets)

	anchorCache := make(map[string]map[string]bool)
	type fullBroken struct {
		path   string
		line   int
		target string
		err    string
	}
	var allBroken []fullBroken
	totalChecked := 0

	for _, t := range targets {
		chk, brk := checkLinks(t, repoRoot, anchorCache)
		totalChecked += chk
		for _, b := range brk {
			allBroken = append(allBroken, fullBroken{
				path:   t,
				line:   b.line,
				target: b.target,
				err:    b.err,
			})
		}
	}

	if len(allBroken) > 0 {
		for _, b := range allBroken {
			rel, _ := filepath.Rel(repoRoot, b.path)
			fmt.Fprintf(os.Stderr, "%s:%d: broken link '%s' -> %s\n", rel, b.line, b.target, b.err)
		}
		os.Exit(1)
	}

	if !*quiet {
		fmt.Printf("0 broken links (%d checked across %d files).\n", totalChecked, len(targets))
	}
}
