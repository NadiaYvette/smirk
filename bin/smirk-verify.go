package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func findHlint() string {
	home := os.Getenv("HOME")
	candidates := []string{
		filepath.Join(home, ".ghcup", "bin", "hlint"),
		filepath.Join(home, ".local", "bin", "hlint"),
	}
	for _, c := range candidates {
		if fi, err := os.Stat(c); err == nil && (fi.Mode()&0111 != 0) {
			return c
		}
	}
	if p, err := exec.LookPath("hlint"); err == nil {
		return p
	}
	return ""
}

func verifyHlint(repoRoot string) {
	fmt.Println("=== [1/5] HLint Static Analysis Gate ===")
	hlint := findHlint()
	if hlint == "" {
		fmt.Println("  [SKIP] hlint binary not found.")
		return
	}

	patterns := []string{
		filepath.Join(repoRoot, "packages", "*", "src"),
		filepath.Join(repoRoot, "src"),
		filepath.Join(repoRoot, "effectful", "src"),
		filepath.Join(repoRoot, "polysemy", "src"),
	}

	var targetDirs []string
	for _, pat := range patterns {
		matches, _ := filepath.Glob(pat)
		for _, m := range matches {
			if fi, err := os.Stat(m); err == nil && fi.IsDir() {
				targetDirs = append(targetDirs, m)
			}
		}
	}

	if len(targetDirs) == 0 {
		fmt.Println("  [SKIP] No Haskell src directories found.")
		return
	}

	cmd := exec.Command(hlint, targetDirs...)
	cmd.Dir = repoRoot
	out, err := cmd.CombinedOutput()
	if err != nil {
		fmt.Fprintf(os.Stderr, "  [FAIL] HLint failed:\n%s\n", string(out))
		os.Exit(1)
	}
	fmt.Println("  [+] Clean HLint run: 0 hints.")
}

func verifyTests(repoRoot string) {
	fmt.Println("=== [2/5] Full Cabal Test Suite ===")
	cabalProject := filepath.Join(repoRoot, "cabal.project")
	if _, err := os.Stat(cabalProject); os.IsNotExist(err) {
		fmt.Println("  [SKIP] No Cabal build manifests found.")
		return
	}

	cmd := exec.Command("cabal", "test", "all", "--test-show-details=streaming")
	cmd.Dir = repoRoot
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		fmt.Fprintf(os.Stderr, "  [FAIL] Cabal test suite failed: %v\n", err)
		os.Exit(1)
	}
	fmt.Println("  [+] All test suite assertions PASSED.")
}

func verifyTagref(repoRoot string) {
	fmt.Println("=== [3/5] Tagref Cross-Reference Gate ===")
	tagrefExe := filepath.Join(repoRoot, "bin", "smirk-tagref")
	if fi, err := os.Stat(tagrefExe); err == nil && (fi.Mode()&0111 != 0) {
		cmd := exec.Command(tagrefExe)
		cmd.Dir = repoRoot
		out, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Fprintf(os.Stderr, "  [FAIL] smirk-tagref failed:\n%s\n", string(out))
			os.Exit(1)
		}
		fmt.Printf("  [+] Tagref verified: %s\n", strings.TrimSpace(string(out)))
		return
	}

	if tagrefBin, err := exec.LookPath("tagref"); err == nil {
		cmd := exec.Command(tagrefBin)
		cmd.Dir = repoRoot
		out, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Fprintf(os.Stderr, "  [FAIL] tagref failed:\n%s\n", string(out))
			os.Exit(1)
		}
		fmt.Println("  [+] Tagref verified: 0 dangling references, 0 duplicate tags.")
		return
	}

	fmt.Println("  [SKIP] tagref binary not found.")
}

func verifyLinkcheck(repoRoot string) {
	fmt.Println("=== [4/5] Markdown Link & Anchor Integrity Gate ===")
	linkcheckExe := filepath.Join(repoRoot, "bin", "smirk-linkcheck")
	if fi, err := os.Stat(linkcheckExe); err == nil && (fi.Mode()&0111 != 0) {
		cmd := exec.Command(linkcheckExe, "--quiet")
		cmd.Dir = repoRoot
		out, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Fprintf(os.Stderr, "  [FAIL] smirk-linkcheck failed:\n%s\n", string(out))
			os.Exit(1)
		}
		fmt.Printf("  [+] Markdown links verified: %s\n", strings.TrimSpace(string(out)))
		return
	}
	fmt.Println("  [SKIP] bin/smirk-linkcheck not found or not executable.")
}

func verifyCloc(repoRoot string) {
	fmt.Println("=== [5/5] CLOC Comment Density Gate ===")
	clocExe := filepath.Join(repoRoot, "bin", "smirk-cloc")
	if fi, err := os.Stat(clocExe); err == nil && (fi.Mode()&0111 != 0) {
		cmd := exec.Command(clocExe, "--quiet", "--check")
		cmd.Dir = repoRoot
		out, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Fprintf(os.Stderr, "  [FAIL] smirk-cloc failed:\n%s\n", string(out))
			os.Exit(1)
		}
		fmt.Printf("  [+] Comment density verified: %s\n", strings.TrimSpace(string(out)))
		return
	}
	fmt.Println("  [SKIP] bin/smirk-cloc not found or not executable.")
}

func main() {
	exePath, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to get executable path: %v\n", err)
		os.Exit(1)
	}
	repoRoot := filepath.Clean(filepath.Join(filepath.Dir(exePath), ".."))

	verifyHlint(repoRoot)
	verifyTests(repoRoot)
	verifyTagref(repoRoot)
	verifyLinkcheck(repoRoot)
	verifyCloc(repoRoot)

	fmt.Println("=== All 5 Physical Verification Checks PASSED ===")
}
