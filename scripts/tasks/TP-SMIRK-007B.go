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
	fmt.Println("=== [TP-SMIRK-007B Task Runner (Go)] ===")

	// 1. Copy DPOMined.hs
	srcMined := findSeed("TP-SMIRK-007B-DPOMined.hs")
	dstMined := "packages/smirk/src/Smirk/Core/DPO/Mined.hs"
	if err := copyFile(srcMined, dstMined); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcMined, dstMined, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstMined)

	// 2. Copy DPOMinedSpec.hs
	srcSpec := findSeed("TP-SMIRK-007B-DPOMinedSpec.hs")
	dstSpec := "packages/smirk/test/DPOMinedSpec.hs"
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
	if !strings.Contains(cabalStr, "Smirk.Core.DPO.Mined") {
		cabalStr = strings.Replace(cabalStr,
			"        Smirk.Sheaf.Parallel\n",
			"        Smirk.Sheaf.Parallel\n        Smirk.Core.DPO.Mined\n", 1)
	}
	if !strings.Contains(cabalStr, "DPOMinedSpec") {
		cabalStr = strings.Replace(cabalStr,
			"    other-modules:\n",
			"    other-modules:\n        DPOMinedSpec\n", 1)
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
	if !strings.Contains(smirkStr, "Smirk.Core.DPO.Mined") {
		smirkStr = strings.Replace(smirkStr,
			"import Smirk.Sheaf.Parallel\n",
			"import Smirk.Sheaf.Parallel\nimport Smirk.Core.DPO.Mined\n", 1)
		smirkStr = strings.Replace(smirkStr,
			"  , module Smirk.Sheaf.Parallel\n",
			"  , module Smirk.Sheaf.Parallel\n  , module Smirk.Core.DPO.Mined\n", 1)
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
	if !strings.Contains(specStr, "DPOMinedSpec") {
		specStr = strings.Replace(specStr,
			"import qualified SheafParallelSpec\n",
			"import qualified SheafParallelSpec\nimport qualified DPOMinedSpec\n", 1)
		specStr = strings.Replace(specStr,
			"  SheafParallelSpec.spec\n",
			"  SheafParallelSpec.spec\n  DPOMinedSpec.spec\n", 1)
		if err := os.WriteFile(specPath, []byte(specStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", specPath, err)
			os.Exit(1)
		}
		fmt.Println("  [+] Updated test/Spec.hs")
	}

	fmt.Println("=== [TP-SMIRK-007B Task Runner Complete] ===")
}
