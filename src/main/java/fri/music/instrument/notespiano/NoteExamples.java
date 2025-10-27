package fri.music.instrument.notespiano;

public interface NoteExamples
{
    public record Notes(String title, Type type, String notes)
    {
        /** Type of sequence. */
        public enum Type
        {
            /** Type for short motives. */
            Motive,
            /** Type for short chords. */
            Chord,
            /** Type for scales. */
            Scale,
            /** Type for songs. */
            Melody,
        }
    }
    
    // motives
    
    Notes MOTIVE_1 = new Notes("G-E", Notes.Type.Motive, """
G4/2 E4/2
""");
    
    Notes MOTIVE_2 = new Notes("G-C", Notes.Type.Motive, """
G4/2 C5/2
""");
    
    Notes MOTIVE_3 = new Notes("C-D-F", Notes.Type.Motive, """
C4/4 D4/4 F4/2
""");
    
    Notes MOTIVE_4 = new Notes("D-F-G", Notes.Type.Motive, """
D4/4 F4/4 G4/2
""");
    
    Notes MOTIVE_5 = new Notes("E-F-G-C", Notes.Type.Motive, """
144
E4/8 F4/8 G4/8 (C5/8 C5/2)
""");
    
    Notes MOTIVE_6 = new Notes("F-E-C-G", Notes.Type.Motive, """
144
F4/8 E4/8 C4/8 (G3/8 G3/2)
""");
    
    // chords
    
    Notes C_CHORD = new Notes("C Major", Notes.Type.Chord, """
C4/4 E4/4 G4/4
""");
    
    Notes AM_CHORD = new Notes("A Minor", Notes.Type.Chord, """
A3/4 C4/4 E4/4
""");
    
    Notes G_DIM_CHORD = new Notes("G Diminished", Notes.Type.Chord, """
B3/4 D4/4 F4/4
""");
    
    Notes G_AUG_CHORD = new Notes("G Augmented", Notes.Type.Chord, """
G3/4 B3/4 D#4/4
""");
    
    // scales
    
    Notes PENTATONIC = new Notes("Pentatonic", Notes.Type.Scale, """
3/4
C4/4 D4/4 E4/4 
G4/4 A4/4 C5/4
""");
    
    Notes C_MAJOR_SCALE = new Notes("C Major", Notes.Type.Scale, """
4/4
C4/4 D4/4 E4/4 F4/4 
G4/4 A4/4 B4/4 C5/4
""");
    
    Notes A_MINOR_HARMONIC = new Notes("A Minor Harmonic", Notes.Type.Scale, """
4/4
A4/4 B4/4 C5/4 D5/4 
E5/4 F5/4 G5/4 A5/4
""");
    
    Notes A_MINOR_MELODIC = new Notes("A Minor Melodic", Notes.Type.Scale, """
4/4
A4/4 B4/4 C5/4 D5/4 
E5/4 F5/4 G#5/4 A5/4
""");
    
    Notes CHROMATIC_SCALE = new Notes("12 Tones", Notes.Type.Scale, """
4/4
C4/8 C#4/8 D4/8 D#4/8 E4/8 F4/8 F#4/8 G4/8 
G#4/8 A4/8 A#4/8 B4/8 C5/4      -/4
""");

    // melodies
    
    Notes AUGUSTIN = new Notes("Augustin", Notes.Type.Melody, """
120
3/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 E4/4 C4/4 C4/4
G4/4. A4/8 G4/8 F4/8 E4/4 C4/4 C4/4
D4/4 G3/4 G3/4 C4/2.
""");
    
    Notes WHEN_THE_SAINTS = new Notes("When the Saints", Notes.Type.Melody, """
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
    
    Notes ODE_TO_JOY = new Notes("Ode to Joy", Notes.Type.Melody, """
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
    
    Notes TURKISH_MARCH = new Notes("Turkish March", Notes.Type.Melody, """
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
    
    Notes WEDDING_MARCH = new Notes("Wedding March", Notes.Type.Melody, """
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
    
    Notes MARSEILLAISE = new Notes("Marseillaise", Notes.Type.Melody, """
120
4/4
-/2 -/8 -/16 D4/16 D4/8. D4/16
G4/4 G4/4 A4/4 A4/4
D5/4. B4/8 G4/8. G4/16 B4/8. G4/16
E4/4 C5/2 A4/8. F#4/16
G4/2 -/2
""");
    
    Notes BLUES = new Notes("Blues", Notes.Type.Melody, """
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
    Notes[] NOTES = new Notes[] {
            MOTIVE_1,
            MOTIVE_2,
            MOTIVE_3,
            MOTIVE_4,
            MOTIVE_5,
            MOTIVE_6,
            C_CHORD,
            AM_CHORD,
            G_DIM_CHORD,
            G_AUG_CHORD,
            PENTATONIC,
            C_MAJOR_SCALE,
            A_MINOR_HARMONIC,
            A_MINOR_MELODIC,
            CHROMATIC_SCALE,
            AUGUSTIN,
            ODE_TO_JOY,
            WEDDING_MARCH,
            TURKISH_MARCH,
            MARSEILLAISE,
            WHEN_THE_SAINTS,
            BLUES
        };
}