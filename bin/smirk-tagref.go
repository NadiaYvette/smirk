package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
)

var (
	tagRegex    = regexp.MustCompile(`\[tag:([a-zA-Z0-9_\-]+)\]`)
	refRegex    = regexp.MustCompile(`\[ref:([a-zA-Z0-9_\-]+)\]`)
	ignoreRules = []string{"bin/smirk-tagref", ".githooks", "scripts", "plans"}
)

func main() {
	exePath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to get executable path: %v\n", err)
		os.Exit(1)
	}
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(exePath), ".."))

	if tagrefBin, err := exec.LookPath("tagref"); err == nil {
		cmd := exec.Command(tagrefBin, os.Args[1:]...)
		cmd.Dir = repoRoot
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				os.Exit(exitErr.ExitCode())
			}
			os.Exit(1)
		}
		return
	}

	// Fallback pure Go tagref scanner
	var gitFiles []string
	gitCmd := exec.Command("git", "ls-files")
	gitCmd.Dir = repoRoot
	if out, err := gitCmd.Output(); err == nil {
		lines := strings.Split(string(out), "\n")
		for _, l := range lines {
			l = strings.TrimSpace(l)
			if l != "" {
				gitFiles = append(gitFiles, l)
			}
		}
	}

	if len(gitFiles) == 0 {
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
			rel, _ := filepath.Rel(repoRoot, p)
			gitFiles = append(gitFiles, rel)
			return nil
		})
	}

	tags := make(map[string]string)
	type refEntry struct {
		target string
		loc    string
	}
	var refs []refEntry
	scanned := 0

	for _, relPath := range gitFiles {
		skip := false
		for _, rule := range ignoreRules {
			r := strings.TrimSuffix(rule, "/")
			if relPath == r || strings.HasPrefix(relPath, r+"/") {
				skip = true
				break
			}
		}
		if skip {
			continue
		}

		absPath := filepath.Join(repoRoot, relPath)
		file, err := os.Open(absPath)
		if err != nil {
			continue
		}
		scanned++

		reader := bufio.NewReader(file)
		lineno := 0
		for {
			line, err := reader.ReadString('\n')
			if err != nil && err != io.EOF {
				break
			}
			lineno++

			for _, m := range tagRegex.FindAllStringSubmatch(line, -1) {
				t := m[1]
				if prev, exists := tags[t]; exists {
					fmt.Fprintf(os.Stderr, "Error: duplicate tag '%s' in %s:%d (previously %s)\n", t, relPath, lineno, prev)
					os.Exit(1)
				}
				tags[t] = fmt.Sprintf("%s:%d", relPath, lineno)
			}

			for _, m := range refRegex.FindAllStringSubmatch(line, -1) {
				refs = append(refs, refEntry{
					target: m[1],
					loc:    fmt.Sprintf("%s:%d", relPath, lineno),
				})
			}

			if err == io.EOF {
				break
			}
		}
		file.Close()
	}

	var dangling []string
	for _, r := range refs {
		if _, ok := tags[r.target]; !ok {
			dangling = append(dangling, fmt.Sprintf("Error: dangling reference '%s' at %s", r.target, r.loc))
		}
	}

	if len(dangling) > 0 {
		for _, d := range dangling {
			fmt.Fprintln(os.Stderr, d)
		}
		os.Exit(1)
	}

	fmt.Printf("%d tags, 0 group members, %d references validated in %d files.\n", len(tags), len(refs), scanned)
}
