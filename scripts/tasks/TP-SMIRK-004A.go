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
	fmt.Println("=== [TP-SMIRK-004A Task Runner] ===")
	cwd, err := os.Getwd()
	if err != nil {
		panic(err)
	}

	seedsDir := filepath.Join(cwd, "seeds")
	if _, err := os.Stat(seedsDir); os.IsNotExist(err) {
		seedsDir = "/home/nyc/src/plans/seeds"
	}

	coopSeed := filepath.Join(seedsDir, "TP-SMIRK-004A-Cooperative.hs")
	specSeed := filepath.Join(seedsDir, "TP-SMIRK-004A-Spec.hs")

	targetCoop := filepath.Join(cwd, "packages", "smirk-penelope", "src", "Smirk", "Continuation", "Cooperative.hs")
	targetSpec := filepath.Join(cwd, "packages", "smirk-penelope", "test", "Spec.hs")

	fmt.Printf("Mounting seed: %s -> %s\n", coopSeed, targetCoop)
	if err := copyFile(coopSeed, targetCoop); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy cooperative module: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Mounting seed: %s -> %s\n", specSeed, targetSpec)
	if err := copyFile(specSeed, targetSpec); err != nil {
		fmt.Fprintf(os.Stderr, "failed to copy spec module: %v\n", err)
		os.Exit(1)
	}

	// Update smirk-penelope.cabal
	cabalPath := filepath.Join(cwd, "packages", "smirk-penelope", "smirk-penelope.cabal")
	cabalBytes, err := os.ReadFile(cabalPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to read %s: %v\n", cabalPath, err)
		os.Exit(1)
	}
	cabalStr := string(cabalBytes)

	if !strings.Contains(cabalStr, "Smirk.Continuation.Cooperative") {
		cabalStr = strings.Replace(
			cabalStr,
			"    exposed-modules:\n        Smirk.Effect.Penelope\n      , Smirk.Effect.Telemetry",
			"    exposed-modules:\n        Smirk.Effect.Penelope\n      , Smirk.Effect.Telemetry\n      , Smirk.Continuation.Cooperative",
			1,
		)
		cabalStr = strings.Replace(
			cabalStr,
			"    build-depends:\n        base >= 4.16 && < 5,",
			"    build-depends:\n        base >= 4.16 && < 5,\n        stm >= 2.5 && < 3,",
			-1,
		)
	}
	if err := os.WriteFile(cabalPath, []byte(cabalStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "failed to write %s: %v\n", cabalPath, err)
		os.Exit(1)
	}

	fmt.Println("=== [TP-SMIRK-004A Setup Complete] ===")
}
