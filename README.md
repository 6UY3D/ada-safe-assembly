# Ada Safe Assembly Compiler Extension

An extension for the Ada GNAT/LLVM backend that introduces a **type-safe assembly language subset**. This project enables developers to write inline assembly in Ada with built-in safeguards against common low-level vulnerabilities such as unsafe register access and buffer overflows. By leveraging a custom Ada-to-LLVM transpiler combined with static analysis passes, this extension enforces robust safety guarantees at compile time.

---

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Getting Started](#getting-started)
  - [Requirements](#requirements)
  - [Installation](#installation)
  - [Usage](#usage)
    
---

## Overview

Traditional inline assembly in many languages poses significant risks, including memory corruption and register mismanagement. This extension redefines inline assembly for Ada by:

- **Enforcing Type-Safety:** Ensuring that assembly instructions respect type constraints and safe memory boundaries.
- **Static Analysis:** Running compile-time analyses to detect and prevent common vulnerabilities like buffer overflows.
- **Seamless Integration:** Embedding safely within Ada code using familiar syntax and developer-friendly error diagnostics.

This project is ideal for developers seeking to blend low-level performance with high-level safety, and it stands as a prime example of modern compiler technology.

---

## Features

- **Safe Assembly Subset:** A well-defined subset of assembly instructions that are statically verified for safety.
- **Custom Transpiler:** Transforms Ada safe assembly blocks into secure LLVM Intermediate Representation (IR).
- **Static Analysis Passes:** Built-in LLVM passes to check for:
  - Buffer overflows and out-of-bounds memory accesses.
  - Improper register usage and type mismatches.
- **Enhanced Error Reporting:** Detailed compile-time error messages to help developers quickly identify and fix unsafe code.
- **Modular and Extensible Architecture:** Designed for easy integration with other tools and further extension.

---

## Getting Started

### Requirements

- **GNAT Ada Compiler:** Ensure you have a recent version of GNAT installed.
- **LLVM Toolchain:** LLVM (version 10 or later recommended) for IR generation and static analysis.
- **CMake:** For building the project.
- **Git:** To clone the repository and manage version control.

### Installation

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/6UY3D/ada-safe-assembly.git
   cd ada-safe-assembly
   ```
2. **Configure the Build:**
   ```bash
   mkdir build && cd build
   cmake ..
   ```
3. **Build the Project:**
   ```bash
   make
   ```
4. **Install (Optional):**
   ```bash
   sudo make install
   ```
### Usage
After installation, you can start using the safe assembly extension in your Ada projects. Below is a simple example illustrating how to annotate and use a safe assembly block:
   ```ada
   with Ada.Text_IO; use Ada.Text_IO;

   procedure Safe_Assembly_Demo is
      pragma Safe_Assembly_Begin;
      -- Safe assembly block: Only allowed instructions with type-safety checks
      -- Example: Move a value from one register to another with safety guarantees
      asm
         "MOV R1, R2"
         -- The above instruction is statically analyzed to ensure:
         -- 1. Correct register usage.
         -- 2. Proper type alignment.
         -- 3. No memory safety issues.
      end asm;
      pragma Safe_Assembly_End;

   begin
      Put_Line("Safe Assembly Demo Completed.");
   end Safe_Assembly_Demo;
   ```
