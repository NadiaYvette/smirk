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
	fmt.Println("=== [TP-SMIRK-006C Task Runner (Go)] ===")

	// 1. Copy DPO.hs
	srcDPO := findSeed("TP-SMIRK-006C-DPO.hs")
	dstDPO := "packages/smirk/src/Smirk/Core/DPO.hs"
	if err := copyFile(srcDPO, dstDPO); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy %s -> %s: %v\n", srcDPO, dstDPO, err)
		os.Exit(1)
	}
	fmt.Printf("  [+] Mounted %s\n", dstDPO)

	// 2. Copy DPOSpec.hs
	srcSpec := findSeed("TP-SMIRK-006C-DPOSpec.hs")
	dstSpec := "packages/smirk/test/DPOSpec.hs"
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
	if !strings.Contains(cabalStr, "Smirk.Core.DPO") {
		cabalStr = strings.Replace(cabalStr,
			"        Smirk.Core.Marpa\n",
			"        Smirk.Core.Marpa\n        Smirk.Core.DPO\n", 1)
	}
	if !strings.Contains(cabalStr, "DPOSpec") {
		cabalStr = strings.Replace(cabalStr,
			"    other-modules:\n",
			"    other-modules:\n        DPOSpec\n", 1)
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
	if !strings.Contains(smirkStr, "Smirk.Core.DPO") {
		smirkStr = strings.Replace(smirkStr,
			"import Smirk.Core.Marpa\n",
			"import Smirk.Core.Marpa\nimport Smirk.Core.DPO\n", 1)
		smirkStr = strings.Replace(smirkStr,
			"  , module Smirk.Core.Marpa\n",
			"  , module Smirk.Core.Marpa\n  , module Smirk.Core.DPO\n", 1)
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
	if !strings.Contains(specStr, "DPOSpec") {
		specStr = strings.Replace(specStr,
			"import qualified MarpaSpec\n",
			"import qualified MarpaSpec\nimport qualified DPOSpec\n", 1)
		specStr = strings.Replace(specStr,
			"  MarpaSpec.spec\n",
			"  MarpaSpec.spec\n  DPOSpec.spec\n", 1)
		if err := os.WriteFile(specPath, []byte(specStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", specPath, err)
			os.Exit(1)
		}
		fmt.Println("  [+] Updated test/Spec.hs")
	}

	fmt.Println("=== [TP-SMIRK-006C Task Runner Complete] ===")
}
