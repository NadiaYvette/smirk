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
	fmt.Println("=== [TP-SMIRK-005C Task Runner (Go)] ===")

	// 1. Copy Zsh/Expansion/Syntax.hs
	srcSyntax := findSeed("TP-SMIRK-005C-ZshSyntax.hs")
	dstSyntax := "packages/smirk/src/Smirk/Zsh/Expansion/Syntax.hs"
	if err := copyFile(srcSyntax, dstSyntax); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcSyntax, dstSyntax, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstSyntax)

	// 2. Copy ZshExpansionSyntaxSpec.hs
	srcSpec := findSeed("TP-SMIRK-005C-ZshSyntaxSpec.hs")
	dstSpec := "packages/smirk/test/ZshExpansionSyntaxSpec.hs"
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
	if !strings.Contains(cabalStr, "Smirk.Zsh.Expansion.Syntax") {
		cabalStr = strings.Replace(cabalStr,
			"        Smirk.Glob\n",
			"        Smirk.Glob\n        Smirk.Zsh.Expansion.Syntax\n", 1)
	}
	if !strings.Contains(cabalStr, "ZshExpansionSyntaxSpec") {
		cabalStr = strings.Replace(cabalStr,
			"    other-modules:\n",
			"    other-modules:\n        ZshExpansionSyntaxSpec\n", 1)
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
	if !strings.Contains(smirkStr, "module Smirk.Zsh.Expansion.Syntax") {
		smirkStr = strings.Replace(smirkStr,
			"  , matchGlob\n",
			"  , matchGlob\n  , module Smirk.Zsh.Expansion.Syntax\n", 1)
	}
	if !strings.Contains(smirkStr, "import Smirk.Zsh.Expansion.Syntax") {
		smirkStr = strings.Replace(smirkStr,
			"import Smirk.Glob\n",
			"import Smirk.Glob\nimport Smirk.Zsh.Expansion.Syntax\n", 1)
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
	if !strings.Contains(specStr, "import qualified ZshExpansionSyntaxSpec") {
		specStr = strings.Replace(specStr,
			"import qualified GlobSpec\n",
			"import qualified GlobSpec\nimport qualified ZshExpansionSyntaxSpec\n", 1)
	}
	if !strings.Contains(specStr, "ZshExpansionSyntaxSpec.spec") {
		specStr = strings.Replace(specStr,
			"  GlobSpec.spec\n",
			"  GlobSpec.spec\n  ZshExpansionSyntaxSpec.spec\n", 1)
	}
	if err := os.WriteFile(specPath, []byte(specStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", specPath, err)
		os.Exit(1)
	}
	fmt.Println("  [+] Updated packages/smirk/test/Spec.hs")

	fmt.Println("=== [TP-SMIRK-005C Task Runner Completed Successfully] ===")
}
