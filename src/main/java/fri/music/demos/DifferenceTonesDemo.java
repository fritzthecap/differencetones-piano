package fri.music.demos;

import java.text.NumberFormat;
import java.util.Arrays;
import java.util.List;
import fri.music.EqualTemperament;
import fri.music.JustIntonation;
import fri.music.JustIntonation.ChromaticScales;
import fri.music.Tone;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.differencetones.DifferenceTones;
import fri.music.player.Note;
import fri.music.player.Player;
import fri.music.wavegenerator.SineWaveSoundChannel;

public class DifferenceTonesDemo
{
    public static void main(String[] args) {
        //final ToneSystem toneSystem = new EqualTemperament();
        final ToneSystem toneSystem = new JustIntonation();
        //final ToneSystem toneSystem = new JustIntonation(ChromaticScales.HARMONIC_SERIES);
        //final ToneSystem toneSystem = new JustIntonation(ChromaticScale.PYTHAGOREAN);
        //final ToneSystem toneSystem = new JustIntonation("A#0", ChromaticScale.LIMIT_5_SYMMETRIC_1);
        //final ToneSystem toneSystem = new JustIntonation(442.0, "B0");

        boolean isEdo12 = toneSystem.name().startsWith(EqualTemperament.class.getSimpleName());
        final double differenceToneDeviationTolerance = 
                isEdo12 ? DifferenceTones.TOLERANT_DEVIATION_EDO_12 : DifferenceTones.PRECISE_DEVIATION_JI;
        System.out.println("Finding difference tones with deviation tolerance = "+differenceToneDeviationTolerance);
        
        System.out.println(toneSystem.name());
    
        final Tone[] tones = toneSystem.tones();
        final boolean onlyPrimaryDifferenceTones = false;
        final DifferenceTones differenceTones = 
                new DifferenceTones(tones, differenceToneDeviationTolerance, onlyPrimaryDifferenceTones);
        
        if (args.length > 0 && args.length % 2 == 0) {
            for (int i = 0; i < args.length; i += 2) {
                final String firstIpnName = args[i];
                final String secondIpnName = args[i + 1];
                
                final Tone firstTone = differenceTones.forIpnName(firstIpnName);
                final Tone secondTone = differenceTones.forIpnName(secondIpnName);
                final Tone[] tartiniTones = differenceTones.findDifferenceTones(firstTone, secondTone);
                
                printNotes(firstTone, secondTone, tartiniTones, onlyPrimaryDifferenceTones);
                
                playNotes(tones, firstTone, secondTone, tartiniTones);
            }
        }
        else { // present difference tones of all intervals in one octave
            //for (Tone tone : tones)
            //    System.out.println(tone);
            
            final List<String> ipnNamesLow = Arrays.asList(
                    "C5", "C#5", "D5", "D#5", "E5", "F5", "F#5", "G5", "G#5", "A5", "A#5", "B5");
            final List<String> ipnNames = Arrays.asList(
                    "C5", "C#5", "D5", "D#5", "E5", "F5", "F#5", "G5", "G#5", "A5", "A#5", "B5",
                    "C6", "C#6", "D6", "D#6", "E6", "F6", "F#6", "G6", "G#6", "A6", "A#6", "B6");
            
            for (int semitones = 1; semitones < ToneSystem.SEMITONES_PER_OCTAVE; semitones++) { // loop most intervals (tone distances)
                System.out.println("======= "+ToneSystem.intervalName(semitones)+" =======");
                
                for (final String lowerIpnName : ipnNamesLow) { // loop all tones, adding and measuring current interval
                    final int lowerIndex = ipnNames.indexOf(lowerIpnName);
                    final String upperIpnName = ipnNames.get(lowerIndex + semitones);
                    
                    final Tone firstTone = differenceTones.forIpnName(lowerIpnName);
                    final Tone secondTone = differenceTones.forIpnName(upperIpnName);
                    final Tone[] tartiniTones = differenceTones.findDifferenceTones(lowerIpnName, upperIpnName);
                    
                    printNotes(firstTone, secondTone, tartiniTones, onlyPrimaryDifferenceTones);
                    System.out.println("----------------------------");
                }
            }
        }
    }

    private static void printNotes(Tone firstTone, Tone secondTone, Tone[] tartiniTones, boolean justFirst) {
        final NumberFormat format = Tone.frequencyFormat;
        
        System.out.println("Difference-tone for ");
        System.out.println("  "+firstTone);
        System.out.println("  "+secondTone);
        
        final double frequencyDifference = firstTone.frequency - secondTone.frequency;
        final String frequencyDifferenceString = format.format(Math.abs(frequencyDifference));
        final double deviation = (tartiniTones[0] != null) 
                ? tartiniTones[0].frequency - Math.abs(frequencyDifference)
                : 0.0;
        final String deviationString = format.format(Math.abs(deviation));
        final boolean deviationIsNull = (Double.valueOf(deviationString) == 0);
        final String deviationDisplay = deviationIsNull ? "" : "("+(deviation >= 0.0 ? "+" : "-")+deviationString+")";

        System.out.println("with difference "+
                frequencyDifferenceString+
                " is "+deviationDisplay);
        
        for (int i = 0; i < tartiniTones.length; i++) {
            final Tone tone = tartiniTones[i];
            System.out.println((justFirst ? "  " : (i + 1)+": ")+tone);
            if (justFirst)
                break;
        }
    }

    private static void playNotes(Tone[] tones, Tone firstTone, Tone secondTone, Tone[] tartiniTones) {
        final Player player = new Player(new SineWaveSoundChannel(tones));
        final Tones tuning = new Tones(tones);
        
        player.play(new Note(tuning, firstTone.ipnName, 1000));
        player.play(new Note(tuning, secondTone.ipnName, 1000));
        
        if (tartiniTones[0] == null)
            return;
        
        player.play(new Note(tuning, tartiniTones[0].ipnName, 2000));
        
        final int REPETITIONS = 1;
        for (int i = 0; i < REPETITIONS; i++) {
            player.playSimultaneously(new Note[] { 
                    new Note(tuning, firstTone.ipnName, 3000), 
                    new Note(tuning, secondTone.ipnName, 3000) 
                });
            player.play(new Note(tuning, tartiniTones[0].ipnName, 2000));
        }
        
        player.close();
    }
}