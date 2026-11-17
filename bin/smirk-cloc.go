package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

type counts struct {
	blank   int
	comment int
	code    int
}

func countHs(path string) (counts, error) {
	file, err := os.Open(path)
	if err != nil {
		return counts{}, err
	}
	defer file.Close()

	reader := bufio.NewReader(file)
	var c counts
	inBlock := 0

	for {
		line, err := reader.ReadString('\n')
		if err != nil && err != io.EOF {
			return c, err
		}

		s := strings.TrimSpace(line)
		if len(s) == 0 {
			if inBlock > 0 {
				c.comment++
			} else {
				c.blank++
			}
			if err == io.EOF {
				break
			}
			continue
		}

		lineHasComment := false
		lineHasCode := false
		i := 0
		n := len(s)

		for i < n {
			if inBlock > 0 {
				if i+1 < n && s[i:i+2] == "-}" {
					inBlock--
					i += 2
					lineHasComment = true
				} else if i+1 < n && s[i:i+2] == "{-" {
					inBlock++
					i += 2
					lineHasComment = true
				} else {
					i++
					lineHasComment = true
				}
			} else {
				if i+1 < n && s[i:i+2] == "--" {
					lineHasComment = true
					break
				} else if i+1 < n && s[i:i+2] == "{-" {
					inBlock++
					i += 2
					lineHasComment = true
				} else {
					lineHasCode = true
					i++
				}
			}
		}

		if lineHasComment && !lineHasCode {
			c.comment++
		} else if lineHasCode {
			c.code++
		} else if lineHasComment {
			c.comment++
		} else {
			c.blank++
		}

		if err == io.EOF {
			break
		}
	}

	return c, nil
}

func main() {
	quiet := flag.Bool("quiet", false, "suppress verbose output")
	flag.BoolVar(quiet, "q", false, "suppress verbose output (shorthand)")
	check := flag.Bool("check", true, "fail if density < 25.0%")
	flag.Parse()

	exePath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to get executable path: %v\n", err)
		os.Exit(1)
	}
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(exePath), ".."))

	patterns := []string{
		filepath.Join(repoRoot, "packages", "*", "src"),
		filepath.Join(repoRoot, "src"),
		filepath.Join(repoRoot, "effectful", "src"),
		filepath.Join(repoRoot, "polysemy", "src"),
	}

	var srcFiles []string
	seen := make(map[string]bool)

	for _, pat := range patterns {
		matches, _ := filepath.Glob(pat)
		for _, m := range matches {
			_ = filepath.Walk(m, func(p string, info os.FileInfo, err error) error {
				if err != nil || info.IsDir() {
					return nil
				}
				if strings.HasSuffix(p, ".hs") && !seen[p] {
					seen[p] = true
					srcFiles = append(srcFiles, p)
				}
				return nil
			})
		}
	}
	sort.Strings(srcFiles)

	totComment := 0
	totCode := 0

	for _, f := range srcFiles {
		c, err := countHs(f)
		if err == nil {
			totComment += c.comment
			totCode += c.code
		}
	}

	totAll := totComment + totCode
	var density float64
	if totAll > 0 {
		density = (float64(totComment) / float64(totAll)) * 100.0
	} else {
		density = 100.0
	}

	if *check && density < 25.0 {
		fmt.Fprintf(os.Stderr, "Comment density gate FAILED: %.1f%% < 25.0%% threshold\n", density)
		os.Exit(1)
	}

	if !*quiet {
		fmt.Printf("%.1f%% (%d comments / %d code lines in src/; target: 50.0%%+)\n", density, totComment, totCode)
	}
}
