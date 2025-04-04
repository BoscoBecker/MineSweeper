# Delphi Splash Screen Project

This project demonstrates how to implement a **professional Splash Screen** in Delphi. The splash screen is shown **before the main form is loaded**, making it ideal for displaying logos, version info, or loading messages to improve user experience.

---

## 📦 Project Structure

# Delphi Splash Screen Project

This project demonstrates how to implement a **professional Splash Screen** in Delphi. The splash screen is shown **before the main form is loaded**, making it ideal for displaying logos, version info, or loading messages to improve user experience.
---

## 🚀 How It Works

1. `SplashForm` is created and shown directly in the `.dpr` file.
2. While the splash screen is visible, the app performs loading tasks.
3. After loading is complete, the main form (`MainForm`) is created.
4. The splash screen is closed and freed before `Application.Run`.

---

## 🧱 Requirements

- Delphi 7 or higher (tested on Delphi 10.4, 11 Alexandria, and 12 Athens)
- VCL components only (no external dependencies)

---

## 🧪 Example (`MineSweeper.dpr`)

```pascal
begin
  Application.Initialize;

  SplashForm := TSplashForm.Create(nil);
  SplashForm.Show;
  SplashForm.Update; // Force immediate rendering

  // Simulate loading process (e.g., database, cache, etc.)
  Sleep(2000);

  Application.CreateForm(TMainForm, MainForm);
  SplashForm.Close;
  SplashForm.Free;

  Application.Run;
end.

More : https://minesweeper.boscobecker.fun/
