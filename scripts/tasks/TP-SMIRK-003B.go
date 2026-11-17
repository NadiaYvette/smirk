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
	fmt.Println("=== [TP-SMIRK-003B Task Runner] ===")
	cwd, err := os.Getwd()
	if err != nil {
		panic(err)
	}

	seedsDir := filepath.Join(cwd, "seeds")
	if _, err := os.Stat(seedsDir); os.IsNotExist(err) {
		seedsDir = "/home/nyc/src/plans/seeds"
	}

	penelopeSeed := filepath.Join(seedsDir, "TP-SMIRK-003B-Penelope.hs")
	specSeed := filepath.Join(seedsDir, "TP-SMIRK-003B-Spec.hs")

	targetPenelope := filepath.Join(cwd, "packages", "smirk-penelope", "src", "Smirk", "Effect", "Penelope.hs")
	targetSpec := filepath.Join(cwd, "packages", "smirk-penelope", "test", "Spec.hs")

	fmt.Printf("Mounting seed: %s -> %s\n", penelopeSeed, targetPenelope)
	if err := copyFile(penelopeSeed, targetPenelope); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy penelope module: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Mounting seed: %s -> %s\n", specSeed, targetSpec)
	if err := copyFile(specSeed, targetSpec); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy spec module: %v\n", err)
		os.Exit(1)
	}

	cabalPath := filepath.Join(cwd, "packages", "smirk-penelope", "smirk-penelope.cabal")
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)

	if !strings.Contains(cabalStr, "bytestring") {
		cabalStr = strings.Replace(cabalStr, "        text >= 2.0 && < 3\n", "        text >= 2.0 && < 3,\n        bytestring >= 0.10 && < 0.13\n", 2)
	}
	if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
		os.Exit(1)
	}

	fmt.Println("=== [TP-SMIRK-003B Setup Complete] ===")
}
