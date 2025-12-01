# DifferenceTones-Piano

![Static Badge](https://img.shields.io/badge/Topic-Music-blue?link=https://en.wikipedia.org/wiki/Music)
![Static Badge](https://img.shields.io/badge/Type-Desktop_App-blue?link=https://en.wikipedia.org/wiki/Desktop_computer)
![Static Badge](https://img.shields.io/badge/Language-Java_17-darkgreen?link=https://openjdk.org/)
![Static Badge](https://img.shields.io/badge/UI_System-Swing-darkgreen?link=https://docs.oracle.com/javase/tutorial/uiswing/index.html)
![Static Badge](https://img.shields.io/badge/Application_JAR-2.15_MB-darkgreen)

![Static Badge](https://img.shields.io/github/license/fritzthecap/differencetones-piano?color=pink)
![GitHub Created At](https://img.shields.io/github/created-at/fritzthecap/differencetones-piano?color=pink)
![GitHub code size in bytes](https://img.shields.io/github/languages/code-size/fritzthecap/differencetones-piano?color=pink)
![GitHub repo size](https://img.shields.io/github/repo-size/fritzthecap/differencetones-piano?color=pink)
![GitHub last commit](https://img.shields.io/github/last-commit/fritzthecap/differencetones-piano?color=pink)
![GitHub Downloads (all assets, all releases)](https://img.shields.io/github/downloads/fritzthecap/differencetones-piano/total?color=pink)

Equal temperament, just intonation, screen pianos, frequency sliders, wave generators - an educational Java/Swing project for musicians interested in difference-tones. It also offers a new way for harmonising melodies, including export to ABC notation from where you can print traditional music scores.

## Read

The documentation / manual is an integrated part of the application, available on the main-window and lots of "Help" buttons. Read it online at 
- [src/main/resources/fri/music/introduction.html](https://html-preview.github.io/?url=https://github.com/fritzthecap/differencetones-piano/blob/main/src/main/resources/fri/music/introduction.html)

## Install

To run the _differencetones-piano_ __desktop-application__, you need at least [Java 17](https://openjdk.org/projects/jdk/17/) or better [Java 21](https://openjdk.org/projects/jdk/21/) installed on your computer. Java virtual machines are freely available as "Open Java", the JRE (runtime-environment) is enough, you don't need the JDK (development-kit) unless you want to write new code. Check that the executable interpreter (java.exe or java) is in the execution PATH of your operating-system. Run "java -version" to check your installed Java version. Then download the __differencetones-piano.jar__ either from root directory (latest) or from  [the latest release](https://github.com/fritzthecap/differencetones-piano/releases/tag/v1.3) and launch the application via

    cd myDownloadDirectory
    java -jar differenctones-piano.jar

On some operating-systems it may be possible to run the application by a double click in some file-explorer. There are also _run.sh_ and _run.bat_ platform scripts. 

Mind that this application __does not read/write anything from/to your file-system__ (hard-disk), so please __save your creative work__ by using the text-editor of your choice (that should preserve UNIX-newlines).

## Develop

This is a _Maven 3.6 / Java 21_ project (minimum _Java 17_), developed with _Eclipse 2024-09_. Integration into any Java-able IDE should be easy, there are no external dependencies except _junit-jupiter 6_ in _test_ scope.

## Screenshots

### Rendering and playing difference-tones for intervals in various tunings:

<img width="1203" height="205" alt="Difference-Tones_Piano" src="https://github.com/user-attachments/assets/b6aa0204-28ca-448b-a3ee-b02d4c7404a0" />

----

### A screen piano configured with the Java built-in synthesizer: 

![MidiSynthesizer](https://github.com/user-attachments/assets/950eee6e-7b08-49ba-b56f-7284aece1320)

----

### Frequency sliders with 4 Cent precision for perceiving stepless difference-tones:

<img width="1200" height="410" alt="Difference-Tones_FrequencySlider" src="https://github.com/user-attachments/assets/f80eba18-6290-42f8-a332-8b01ef4baf12" />

----

### Harmonising melodies with difference-tone intervals:

<img width="1206" height="733" alt="Compose_Difference-Tone_Intervals" src="https://github.com/user-attachments/assets/d2d505eb-5934-496e-9a91-2582da4a4daf" />

----

### An ABC export example:
    
<div align="center">
 <img width="750" height="480" alt="ode-to-joy_adjusted-difftones" src="https://github.com/user-attachments/assets/940d3b45-93ea-486f-9a0e-4cff9b1d6baa" />
</div>

----

### The application's main-window:

<div align="center">
<img width="1040" height="700" alt="DifferenceTonesPiano_MainWindow" src="https://github.com/user-attachments/assets/cef339b4-15d8-4024-a9f5-35a7a3b99935" />
</div>



