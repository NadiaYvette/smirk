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
	fmt.Println("=== [TP-SMIRK-005D Task Runner (Go)] ===")

	// 1. Copy Zsh/Expansion/Eval.hs
	srcEval := findSeed("TP-SMIRK-005D-ZshEval.hs")
	dstEval := "packages/smirk-penelope/src/Smirk/Zsh/Expansion/Eval.hs"
	if err := copyFile(srcEval, dstEval); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcEval, dstEval, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstEval)

	// 2. Copy ZshExpansionEvalSpec.hs
	srcSpec := findSeed("TP-SMIRK-005D-ZshEvalSpec.hs")
	dstSpec := "packages/smirk-penelope/test/ZshExpansionEvalSpec.hs"
	if err := copyFile(srcSpec, dstSpec); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcSpec, dstSpec, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstSpec)

	// 3. Update packages/smirk-penelope/smirk-penelope.cabal
	cabalPath := "packages/smirk-penelope/smirk-penelope.cabal"
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)
	if !strings.Contains(cabalStr, "Smirk.Zsh.Expansion.Eval") {
		cabalStr = strings.Replace(cabalStr,
			"      , Smirk.Effect.Callout\n",
			"      , Smirk.Effect.Callout\n      , Smirk.Zsh.Expansion.Eval\n", 1)
	}
	if !strings.Contains(cabalStr, "ZshExpansionEvalSpec") {
		cabalStr = strings.Replace(cabalStr,
			"        CalloutSpec\n",
			"        CalloutSpec\n        ZshExpansionEvalSpec\n", 1)
	}
	if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	fmt.Println("  [+] Updated smirk-penelope.cabal")

	// 4. Update packages/smirk-penelope/test/Spec.hs
	specPath := "packages/smirk-penelope/test/Spec.hs"
	specBytes, err := os.ReadFile(specPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", specPath, err)
		os.Exit(1)
	}
	specStr := string(specBytes)
	if !strings.Contains(specStr, "import qualified ZshExpansionEvalSpec") {
		specStr = strings.Replace(specStr,
			"import qualified CalloutSpec\n",
			"import qualified CalloutSpec\nimport qualified ZshExpansionEvalSpec\n", 1)
	}
	if !strings.Contains(specStr, "ZshExpansionEvalSpec.spec") {
		specStr = strings.Replace(specStr,
			"  CalloutSpec.spec\n",
			"  CalloutSpec.spec\n  ZshExpansionEvalSpec.spec\n", 1)
	}
	if err := os.WriteFile(specPath, []byte(specStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", specPath, err)
		os.Exit(1)
	}
	fmt.Println("  [+] Updated packages/smirk-penelope/test/Spec.hs")

	fmt.Println("=== [TP-SMIRK-005D Task Runner Completed Successfully] ===")
}
