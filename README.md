
<h1 align="center">🎮 ASCII Alphabet Catcher</h1>
<p align="center">
  <i>A classic arcade-style game built in x86 Assembly (16-bit Real Mode)</i>
</p>

---

## ✨ Features
<ul>
  <li>🕹 Moveable box controlled with arrow keys</li>
  <li>🔤 Falling alphabets rendered in text mode</li>
  <li>📈 Real-time score and life tracking</li>
  <li>💀 Game Over condition when lives reach zero</li>
  <li>🔥 Increasing difficulty as score increases</li>
</ul>

---

## ⚙️ Technical Details

<table>
  <tr>
    <td><b>📁 Platform</b></td>
    <td>DOS / Emu8086 / DOSBox (Real Mode)</td>
  </tr>
  <tr>
    <td><b>🧠 Language</b></td>
    <td>x86 Assembly</td>
  </tr>
  <tr>
    <td><b>🖥 Graphics</b></td>
    <td>Text-mode via video memory <code>0xB800</code></td>
  </tr>
  <tr>
    <td><b>⌨️ Input</b></td>
    <td>Keyboard via <code>INT 9h</code></td>
  </tr>
  <tr>
    <td><b>🔄 Randomization</b></td>
    <td>System timer via <code>INT 1Ah</code></td>
  </tr>
</table>

---

## 🕹 Controls

| Key       | Action           |
|-----------|------------------|
| ← Left    | Move box left    |
| → Right   | Move box right   |

---

## 🚀 How to Run

### 💻 Using DOSBox:
```bash
tasm game.asm
tlink game.obj
game.com
```

### 🔧 Using Emu8086:
1. Open `game.asm` in Emu8086.
2. Click **Compile and Run**.

---

## 🧠 Game Logic

- Alphabets fall vertically from the top of the screen.
- Catch them inside the box to increase `score` ✅
- Missing them reduces `lives` ❌
- When `lives == 0` ➡️ `Game Over!` 💀

---

## 🗂 Code Structure

| 🧩 Procedure          | 📌 Purpose                         |
|----------------------|------------------------------------|
| `randG`              | Custom random generator            |
| `draw_box`           | Draws the player's box             |
| `alphabet_thrower`   | Handles the falling alphabet       |
| `left`, `right`      | Player movement                    |
| `new_key_int`        | Keyboard interrupt handler         |
| `Check_Score`        | Updates score and lives            |
| `printstr`, `printnum` | Displays strings and numbers     |

---

## ⚠️ Limitations

- ❌ No restart or pause function
- ❌ Not compatible with modern OS without emulator
- ✅ Only runs in real-mode DOS environments

---

## 🧩 Possible Enhancements

- 🔁 Add levels or increasing difficulty
- 🏆 High score saving
- 🔊 Sound via PC speaker
- 🎨 UI improvements and smoother animations
  
---

## 📄 License

🆓 **Open-source** — Use freely for learning and educational projects.  
📢 Attribution is appreciated!

---

<p align="center"><b>💻 Made with pure x86 Assembly & 💙 passion for retro coding</b></p>﻿# Alphabet_Catcher
