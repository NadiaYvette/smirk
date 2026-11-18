package main

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

func copyFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()

	if err := os.MkdirAll(filepath.Dir(dst), 0755); err != nil {
		return err
	}

	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()

	_, err = io.Copy(out, in)
	return err
}

func findSeed(name string) string {
	candidates := []string{
		filepath.Join("seeds", name),
		filepath.Join(os.Getenv("HOME"), "src/plans/seeds", name),
		filepath.Join("..", "seeds", name),
	}
	for _, c := range candidates {
		if _, err := os.Stat(c); err == nil {
			return c
		}
	}
	return filepath.Join("seeds", name)
}

func main() {
	fmt.Println("=== [TP-SMIRK-005B Task Runner (Go)] ===")

	// 1. Copy Glob/Zsh.hs
	srcGlob := findSeed("TP-SMIRK-005B-GlobZsh.hs")
	dstGlob := "packages/smirk/src/Smirk/Glob/Zsh.hs"
	if err := copyFile(srcGlob, dstGlob); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcGlob, dstGlob, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstGlob)

	// 2. Copy GlobZshSpec.hs
	srcSpec := findSeed("TP-SMIRK-005B-GlobZshSpec.hs")
	dstSpec := "packages/smirk/test/GlobZshSpec.hs"
	if err := copyFile(srcSpec, dstSpec); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcSpec, dstSpec, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstSpec)

	// 3. Update packages/smirk/smirk.cabal
	cabalPath := "packages/smirk/smirk.cabal"
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)
	if !strings.Contains(cabalStr, "Smirk.Glob.Zsh") {
		cabalStr = strings.Replace(cabalStr,
			"        Smirk.Glob\n",
			"        Smirk.Glob\n        Smirk.Glob.Zsh\n", 1)
	}
	if !strings.Contains(cabalStr, "GlobZshSpec") {
		cabalStr = strings.Replace(cabalStr,
			"    other-modules:\n",
			"    other-modules:\n        GlobZshSpec\n", 1)
	}
	if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	fmt.Println("  [+] Updated smirk.cabal")

	// 4. Update packages/smirk/src/Smirk.hs
	smirkPath := "packages/smirk/src/Smirk.hs"
	smirkBytes, err := os.ReadFile(smirkPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", smirkPath, err)
		os.Exit(1)
	}
	smirkStr := string(smirkBytes)
	if !strings.Contains(smirkStr, "module Smirk.Glob.Zsh") {
		smirkStr = strings.Replace(smirkStr,
			"  , matchGlob\n",
			"  , matchGlob\n  , module Smirk.Glob.Zsh\n", 1)
	}
	if !strings.Contains(smirkStr, "import Smirk.Glob.Zsh") {
		smirkStr = strings.Replace(smirkStr,
			"import Smirk.Glob\n",
			"import Smirk.Glob\nimport Smirk.Glob.Zsh\n", 1)
	}
	if err := os.WriteFile(smirkPath, []byte(smirkStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", smirkPath, err)
		os.Exit(1)
	}
	fmt.Println("  [+] Updated packages/smirk/src/Smirk.hs")

	// 5. Update packages/smirk/test/Spec.hs
	specPath := "packages/smirk/test/Spec.hs"
	specBytes, err := os.ReadFile(specPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", specPath, err)
		os.Exit(1)
	}
	specStr := string(specBytes)
	if !strings.Contains(specStr, "import qualified GlobZshSpec") {
		specStr = strings.Replace(specStr,
			"import qualified GlobSpec\n",
			"import qualified GlobSpec\nimport qualified GlobZshSpec\n", 1)
	}
	if !strings.Contains(specStr, "GlobZshSpec.spec") {
		specStr = strings.Replace(specStr,
			"  GlobSpec.spec\n",
			"  GlobSpec.spec\n  GlobZshSpec.spec\n", 1)
	}
	if err := os.WriteFile(specPath, []byte(specStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", specPath, err)
		os.Exit(1)
	}
	fmt.Println("  [+] Updated packages/smirk/test/Spec.hs")

	fmt.Println("=== [TP-SMIRK-005B Task Runner Completed Successfully] ===")
}
