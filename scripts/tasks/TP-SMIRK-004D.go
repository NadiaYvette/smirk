package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func copyFile(src, dst string) error {
	data, err := os.ReadFile(src)
	if err != nil {
		return err
	}
	if err := os.MkdirAll(filepath.Dir(dst), 0755); err != nil {
		return err
	}
	return os.WriteFile(dst, data, 0644)
}

func main() {
	fmt.Println("=== [TP-SMIRK-004D Task Runner] ===")
	cwd, err := os.Getwd()
	if err != nil {
		panic(err)
	}

	seedsDir := filepath.Join(cwd, "seeds")
	if _, err := os.Stat(seedsDir); os.IsNotExist(err) {
		seedsDir = "/home/nyc/src/plans/seeds"
	}

	diSeed := filepath.Join(seedsDir, "TP-SMIRK-004D-DiskIndex.hs")
	specSeed := filepath.Join(seedsDir, "TP-SMIRK-004D-DiskIndexSpec.hs")

	targetDI := filepath.Join(cwd, "packages", "smirk", "src", "Smirk", "Continuation", "DiskIndex.hs")
	targetSpec := filepath.Join(cwd, "packages", "smirk", "test", "DiskIndexSpec.hs")

	fmt.Printf("Mounting seed: %s -> %s\n", diSeed, targetDI)
	if err := copyFile(diSeed, targetDI); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy disk index module: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Mounting seed: %s -> %s\n", specSeed, targetSpec)
	if err := copyFile(specSeed, targetSpec); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy spec module: %v\n", err)
		os.Exit(1)
	}

	// 1. Update packages/smirk/smirk.cabal
	cabalPath := filepath.Join(cwd, "packages", "smirk", "smirk.cabal")
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)
	if !strings.Contains(cabalStr, "Smirk.Continuation.DiskIndex") {
		cabalStr = strings.Replace(
			cabalStr,
			"        Smirk.Continuation.NetworkFilter",
			"        Smirk.Continuation.NetworkFilter\n        Smirk.Continuation.DiskIndex",
			1,
		)
		cabalStr = strings.Replace(
			cabalStr,
			"        NetworkFilterSpec",
			"        NetworkFilterSpec\n        DiskIndexSpec",
			1,
		)
		if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
			os.Exit(1)
		}
	}

	// 2. Update packages/smirk/src/Smirk.hs
	smirkHsPath := filepath.Join(cwd, "packages", "smirk", "src", "Smirk.hs")
	smirkHsBytes, err := os.ReadFile(smirkHsPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", smirkHsPath, err)
		os.Exit(1)
	}
	smirkHsStr := string(smirkHsBytes)
	if !strings.Contains(smirkHsStr, "Smirk.Continuation.DiskIndex") {
		smirkHsStr = strings.Replace(
			smirkHsStr,
			"  , module Smirk.Continuation.NetworkFilter",
			"  , module Smirk.Continuation.NetworkFilter\n  , module Smirk.Continuation.DiskIndex",
			1,
		)
		smirkHsStr = smirkHsStr + "\nimport Smirk.Continuation.DiskIndex\n"
		if err := os.WriteFile(smirkHsPath, []byte(smirkHsStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", smirkHsPath, err)
			os.Exit(1)
		}
	}

	// 3. Update packages/smirk/test/Spec.hs
	specHsPath := filepath.Join(cwd, "packages", "smirk", "test", "Spec.hs")
	specHsBytes, err := os.ReadFile(specHsPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", specHsPath, err)
		os.Exit(1)
	}
	specHsStr := string(specHsBytes)
	if !strings.Contains(specHsStr, "DiskIndexSpec") {
		specHsStr = strings.Replace(
			specHsStr,
			"import qualified NetworkFilterSpec",
			"import qualified NetworkFilterSpec\nimport qualified DiskIndexSpec",
			1,
		)
		specHsStr = strings.Replace(
			specHsStr,
			"  NetworkFilterSpec.spec",
			"  NetworkFilterSpec.spec\n  DiskIndexSpec.spec",
			1,
		)
		if err := os.WriteFile(specHsPath, []byte(specHsStr), 0644); err != nil {
			fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", specHsPath, err)
			os.Exit(1)
		}
	}

	fmt.Println("=== [TP-SMIRK-004D Setup Complete] ===")
}
