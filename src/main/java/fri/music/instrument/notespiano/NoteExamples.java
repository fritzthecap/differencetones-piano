package fri.music.instrument.notespiano;

public interface NoteExamples
{
    public record Melody(String title, boolean isSong, String notes)
    {
        public Melody(String title, String notes) {
            this(title, false, notes);
        }
    }
    
    // patterns
    
    Melody MOTIVE_1 = new Melody("Motive 1", """
D4/4 F4/4 G4/4
""");
    
    Melody MOTIVE_2 = new Melody("Motive 2", """
D4/4 E4/4 G4/4
""");
    
    Melody C_CHORD = new Melody("C Major Chord", """
C4/4 E4/4 G4/4
""");
    
    Melody AM_CHORD = new Melody("A Minor Chord", """
A4/4 E4/4 C4/4
""");
    
    Melody PENTATONIC = new Melody("Pentatonic", """
3/4
C4/4 D4/4 E4/4 
G4/4 A4/4 C5/4
""");
    
    Melody C_MAJOR_SCALE = new Melody("C Major Scale", """
4/4
C4/4 D4/4 E4/4 F4/4 
G4/4 A4/4 B4/4 C5/4
""");
    
    Melody A_MINOR_HARMONIC = new Melody("A Minor Harmonic", """
4/4
A4/4 B4/4 C5/4 D5/4 
E5/4 F5/4 G5/4 A5/4
""");
    
    Melody A_MINOR_MELODIC = new Melody("A Minor Melodic", """
4/4
A4/4 B4/4 C5/4 D5/4 
E5/4 F5/4 G#5/4 A5/4
""");
    
    Melody CHROMATIC_SCALE = new Melody("12 Tone Scale", """
4/4
C4/8 C#4/8 D4/8 D#4/8 E4/8 F4/8 F#4/8 G4/8 
G#4/8 A4/8 A#4/8 B4/8 C5/4      -/4
""");

    // melodies
    
    Melody AUGUSTIN = new Melody("Augustin", true, """
120
3/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 E4/4 C4/4 C4/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 C4/2.
""");
    
    Melody WHEN_THE_SAINTS = new Melody("When the Saints", true, """
120
4/4
-/2 -/8 G4/8 B4/8 C5/8 
(D5/2 D5/8) G4/8 B4/8 C5/8
(D5/2 D5/8) G4/8 B4/8 C5/8
D5/4 B4/4 G4/4 B4/4
A4/2 -/8 B4/8 B4/8 A4/8 
G4/4. G4/8
B4/4  D5/8 D5/8
D5/8 (C5/4. C5/8) C5/8 B4/8 C5/8
D5/4 B4/4 G4/4 A4/4
G4/1 
""");
    
    Melody ODE_TO_JOY = new Melody("Ode to Joy", true, """
120
4/4
A4/4 A4/4 A#4/4 C5/4
C5/4 A#4/4 A4/4 G4/4
F4/4 F4/4 G4/4 A4/4
A4/4. G4/8 G4/2
A4/4 A4/4 A#4/4 C5/4
C5/4 A#4/4 A4/4 G4/4
F4/4 F4/4 G4/4 A4/4
G4/4. F4/8 F4/2
""");
    
    Melody TURKISH_MARCH = new Melody("Turkish March", true, """
180
4/4
-/2 B5/8 A5/8 G#5/8 A5/8
C6/2 D6/8 C6/8 B5/8 C6/8
E6/2 F6/8 E6/8 D#6/8 E6/8
B6/8 A6/8 G#6/8 A6/8 B6/8 A6/8 G#6/8 A6/8
C7/2 A6/4 C7/4
G6/32 A6/32 B6/8. A6/4 G6/4 A6/4
G6/32 A6/32 B6/8. A6/4 G6/4 A6/4
G6/32 A6/32 B6/8. A6/4 G6/4 F#6/4
E6/2
""");
    
    Melody WEDDING_MARCH = new Melody("Wedding March", true, """
100
4/4
G4/4 C5/8. C5/16 C5/2
G4/4 D5/8. B4/16 C5/2
G4/4 C5/8. F5/16 F5/4 E5/8. D5/16
C5/4  B4/8.  C5/16 D5/2
G4/4 C5/8. C5/16 C5/2
G4/4 D5/8. B4/16 C5/2
G4/4 C5/8. E5/16 G5/4 E5/8. C5/16
A4/4  D5/8.  E5/16 C5/2
""");
    
    Melody MARSEILLAISE = new Melody("Marseillaise", true, """
120
4/4
-/2 -/8 -/16 D4/16 D4/8. D4/16
G4/4 G4/4 A4/4 A4/4
D5/4. B4/8 G4/8. G4/16 B4/8. G4/16
E4/4 C5/2 A4/8. F#4/16
G4/2 -/2
""");
    
    Melody BLUES = new Melody("Blues", true, """
120
4/4
C4 E4 G4 E4
F4 A4 C5 A4
C4 E4 G4 A4
A#4 A4 G4 E4
F4 A4 C5 A4
D5 C5 A4 F4
C4 E4 G4 E4
A4 G4 E4 C4
G4 B4 D5 B4
F4 A4 C5 A4
C4 E4 G4 A#4
G4 D4 G3/2
""");

    
    /** The list to be seen in application. */
    Melody[] MELODIES = new Melody[] {
            MOTIVE_1,
            MOTIVE_2,
            C_CHORD,
            AM_CHORD,
            PENTATONIC,
            C_MAJOR_SCALE,
            A_MINOR_HARMONIC,
            A_MINOR_MELODIC,
            CHROMATIC_SCALE,
            
            AUGUSTIN,
            WHEN_THE_SAINTS,
            ODE_TO_JOY,
            TURKISH_MARCH,
            WEDDING_MARCH,
            MARSEILLAISE,
            BLUES
        };
}