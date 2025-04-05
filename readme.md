# 🧨 Mine Sweeper - Delphi VCL Game

This is a simple clone of the classic **Mine Sweeper** game, built using **Delphi VCL**. The goal is to reveal all cells that do not contain mines by using logic and deduction.

![image](https://github.com/user-attachments/assets/2376d754-ab2f-45d5-be0b-696cbb61cb9b)


## 🛠️ Technologies

- **Delphi VCL**
- Object-Oriented Programming (OOP)
- Native Delphi components (TButton, TPanel, TLabel, etc.)
- SQlite

## 🎮 How to Play

1. Open the project in **Delphi** (recommended: Delphi 11 or higher).
2. Compile and run the project.
3. Left-click to reveal a cell.
4. Right-click to flag a suspected mine (🚩).
5. If you click a mine, the game is over.
6. You win when all non-mine cells are revealed.

## 🧩 Features

- Configurable grid sizes (e.g., Easy, Medium, Hard).
- Mine counter.
- Timer.
- Game reset button.
- Flagging system.
- Win/loss detection.
- Clean and intuitive interface using native VCL controls.
- Choose the target platform: **Win32** or **Win64**.

## 📁 Project Structure

Root path: src/
```
├───Assets
├───Connection
├───Const
├───Controller
├───Data
├───Draw
├───Engine
├───Enum
├───Sound
├───View
├───Win32
│   ├───Debug
│   └───Release
└───Win64
    ├───Debug
    └───Release
```



## 🧠 Game Logic

- Mines are randomly distributed when the game starts.
- Each revealed cell shows the number of adjacent mines (0–8).
- Recursive revealing is implemented for empty areas (cells with 0 adjacent mines).
- Flags help the player track suspected mine locations.

## 🖼️ Screenshot

![{236FC188-B549-410B-B003-D40E4665A80B}](https://github.com/user-attachments/assets/45d0b719-e057-47c6-83b7-f837209c3772)


![{2D190C88-B78C-403B-AFD1-5845FE59BBD6}](https://github.com/user-attachments/assets/25bebd51-1f6c-415b-9ca7-4eb27b7ee534)

![{DE733962-2C4E-40B9-9FAE-C030E27E7490}](https://github.com/user-attachments/assets/38712daf-c62d-4b2e-853e-8d16ee702df5)


## 📋 Requirements

- Delphi 10 or newer (can be adapted to earlier versions)
- Windows OS

## 📃 License

This project is open-source and free to use for educational or personal purposes.

👨‍💻 Developed with passion in Delphi ❤️
