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
	fmt.Println("=== [TP-SMIRK-003D Task Runner] ===")
	cwd, err := os.Getwd()
	if err != nil {
		panic(err)
	}

	seedsDir := filepath.Join(cwd, "seeds")
	if _, err := os.Stat(seedsDir); os.IsNotExist(err) {
		seedsDir = "/home/nyc/src/plans/seeds"
	}

	polysemySeed := filepath.Join(seedsDir, "TP-SMIRK-003D-Polysemy.hs")
	specSeed := filepath.Join(seedsDir, "TP-SMIRK-003D-Spec.hs")

	targetPolysemy := filepath.Join(cwd, "packages", "smirk-polysemy", "src", "Smirk", "Effect", "Polysemy.hs")
	targetSpec := filepath.Join(cwd, "packages", "smirk-polysemy", "test", "Spec.hs")

	fmt.Printf("Mounting seed: %s -> %s\n", polysemySeed, targetPolysemy)
	if err := copyFile(polysemySeed, targetPolysemy); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy polysemy module: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Mounting seed: %s -> %s\n", specSeed, targetSpec)
	if err := copyFile(specSeed, targetSpec); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy spec module: %v\n", err)
		os.Exit(1)
	}

	cabalPath := filepath.Join(cwd, "packages", "smirk-polysemy", "smirk-polysemy.cabal")
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)

	if !strings.Contains(cabalStr, "stm >= 2.5") {
		cabalStr = strings.Replace(
			cabalStr,
			"        smirk >= 0.1 && < 0.2,\n        text >= 2.0 && < 3",
			"        smirk >= 0.1 && < 0.2,\n        stm >= 2.5 && < 3,\n        text >= 2.0 && < 3",
			1,
		)
		cabalStr = strings.Replace(
			cabalStr,
			"        smirk-polysemy,\n        text >= 2.0 && < 3",
			"        bytestring >= 0.10 && < 0.13,\n        smirk-polysemy,\n        stm >= 2.5 && < 3,\n        text >= 2.0 && < 3",
			1,
		)
	}
	if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
		os.Exit(1)
	}

	fmt.Println("=== [TP-SMIRK-003D Setup Complete] ===")
}
