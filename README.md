# Voxel Engine – Elm Implementation

## Project Overview
This repository contains the source code for a voxel engine implemented in Elm using Functional Reactive Programming (FRP). Users can interact with a 3D voxel world by adding and deleting blocks in real time. The project demonstrates functional composition, higher-order functions, and immutable state management in Elm.

## Features
- Real-time voxel world interaction
- Add and delete blocks dynamically
- Modular design with reusable functional components
- Built with Elm and compiled to run in a browser

## Requirements
- Elm 0.21.1 or later
- Node.js and npm (optional, for running a local server)

## Getting Started

1. **Clone the repository:**
   ```bash
   git clone https://github.com/rylry/Elm.git
   cd Elm
   ```

2. **Install Elm (if not installed):**
   ```bash
   npm install -g elm
   ```

3. **Install project dependencies:**
   ```bash
   elm install
   ```

4. **Compile and run the project:**
   ```bash
   elm make src/Main.elm --output=main.js
   ```
   Open the `index.html` file in your browser to view the voxel engine.
## Interacting with the Engine
- Use WASD, shift, and space to move
- Add or delete blocks by pressing E and R

## Project Notes
- The code demonstrates key Elm and FRP concepts, including recursion, higher-order functions, and immutable records.
- Designed to highlight how functional programming and FRP can be applied to interactive WebGL

## License
This project is for educational purposes only – no license provided.

---

Rylan Ray – CSC240 Capstone, Fall 2025
