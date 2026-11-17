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
	fmt.Println("=== [TP-SMIRK-006D Task Runner (Go)] ===")

	// 1. Copy SheafParallel.hs
	srcSheaf := findSeed("TP-SMIRK-006D-SheafParallel.hs")
	dstSheaf := "packages/smirk/src/Smirk/Sheaf/Parallel.hs"
	if err := copyFile(srcSheaf, dstSheaf); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcSheaf, dstSheaf, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstSheaf)

	// 2. Copy SheafParallelSpec.hs
	srcSpec := findSeed("TP-SMIRK-006D-SheafParallelSpec.hs")
	dstSpec := "packages/smirk/test/SheafParallelSpec.hs"
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
	if !strings.Contains(cabalStr, "Smirk.Sheaf.Parallel") {
		cabalStr = strings.Replace(cabalStr,
			"        Smirk.Core.DPO\n",
			"        Smirk.Core.DPO\n        Smirk.Sheaf.Parallel\n", 1)
	}
	if !strings.Contains(cabalStr, "SheafParallelSpec") {
		cabalStr = strings.Replace(cabalStr,
			"    other-modules:\n",
			"    other-modules:\n        SheafParallelSpec\n", 1)
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
	if !strings.Contains(smirkStr, "Smirk.Sheaf.Parallel") {
		smirkStr = strings.Replace(smirkStr,
			"import Smirk.Core.DPO\n",
			"import Smirk.Core.DPO\nimport Smirk.Sheaf.Parallel\n", 1)
		smirkStr = strings.Replace(smirkStr,
			"  , module Smirk.Core.DPO\n",
			"  , module Smirk.Core.DPO\n  , module Smirk.Sheaf.Parallel\n", 1)
		if err := os.WriteFile(smirkPath, []byte(smirkStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", smirkPath, err)
			os.Exit(1)
		}
		fmt.Println("  [+] Updated Smirk.hs")
	}

	// 5. Update packages/smirk/test/Spec.hs
	specPath := "packages/smirk/test/Spec.hs"
	specBytes, err := os.ReadFile(specPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", specPath, err)
		os.Exit(1)
	}
	specStr := string(specBytes)
	if !strings.Contains(specStr, "SheafParallelSpec") {
		specStr = strings.Replace(specStr,
			"import qualified DPOSpec\n",
			"import qualified DPOSpec\nimport qualified SheafParallelSpec\n", 1)
		specStr = strings.Replace(specStr,
			"  DPOSpec.spec\n",
			"  DPOSpec.spec\n  SheafParallelSpec.spec\n", 1)
		if err := os.WriteFile(specPath, []byte(specStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", specPath, err)
			os.Exit(1)
		}
		fmt.Println("  [+] Updated test/Spec.hs")
	}

	fmt.Println("=== [TP-SMIRK-006D Task Runner Complete] ===")
}
