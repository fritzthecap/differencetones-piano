# differencetones-piano
Equal temperament, just intonation, screen pianos, frequency sliders, wave generators - an educational Java/Swing project for musicians interested in difference tones. It also offers a new way for harmonising melodies, including export to ABC notation from where you can print traditional music scores.

The documentation is also available inside the application, read it online at 
- [src/main/resources/fri/music/introduction.html](https://html-preview.github.io/?url=https://github.com/fritzthecap/differencetones-piano/blob/main/src/main/resources/fri/music/introduction.html).

To run this __desktop-application__, you need at least [Java 17](https://openjdk.org/projects/jdk/17/) or better [Java 21](https://openjdk.org/projects/jdk/21/) installed on your computer. Java virtual machines are freely available as "Open Java", the JRE (runtime-environment) is enough, don't need JDK (development kit) unless you want to write new code. Check that the executable interpreter (java.exe or java) is in the execution PATH of your computer. Run "java --version" to check your installed Java version. Then download the __differencetones-piano-1.0.jar__ file to your computer and launch the application via

    java -jar differenctones-piano-1.0.jar

On WINDOWS it may be possible to run the application by a double click in file-explorer. There are also _run.sh_ and _run.bat_ scripts. Please mind that this application does not read/write anything from/to your file-system (hard-disk), so please save your creative work by using the text-editor of your choice (that should preserve UNIX-newlines).

----

This is a Maven 3.6 / Java 21 project (minimum Java 17), developed on LINUX 6.8.0 with Eclipse 2024-09. Integration into any Java-able IDE should be easy, there are no external dependencies except junit-jupiter 6 in test-scope.

----

__Screenshots__

Displaying and playing difference-tones for intervals in various tunings:

<img width="1203" height="205" alt="Difference-Tones_Piano" src="https://github.com/user-attachments/assets/85f62dba-13df-4cfb-bc00-387cfeaa2634" />

<p/>
<p/>
A screen piano configured with the Java synthesizer: 

![MidiSynthesizer](https://github.com/user-attachments/assets/950eee6e-7b08-49ba-b56f-7284aece1320)

<p/>
Frequency sliders with 4 Cent precision for perceiving stepless difference tones:

![FrequencySliders](https://github.com/user-attachments/assets/22a1dcb5-e046-4a06-a44e-6e9e2b192cff)

<p/>
Harmonising melodies with difference-tone intervals:

<img width="1206" height="733" alt="Compose_Difference-Tone_Intervals" src="https://github.com/user-attachments/assets/d2d505eb-5934-496e-9a91-2582da4a4daf" />

<p/>
An ABC export example:
    
<div align="center">
 <img width="750" height="480" alt="ode-to-joy_adjusted-difftones" src="https://github.com/user-attachments/assets/940d3b45-93ea-486f-9a0e-4cff9b1d6baa" />
</div>

<p/>
The application logo:

<div align="center">
  <img width="592" height="482" alt="theater-moiree_cover" src="https://github.com/user-attachments/assets/ffa3752d-51ea-4f04-96b2-e644a98ed002" />
</div>
