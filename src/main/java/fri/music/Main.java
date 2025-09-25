package fri.music;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.net.URL;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.instrument.notespiano.HelpForNotes;
import fri.music.instrument.notespiano.NoteExamples;
import fri.music.instrument.notespiano.NotesPianoPlayer;
import fri.music.instrument.notespiano.wave.HelpForAutoCompose;
import fri.music.instrument.notespiano.wave.HelpForCompose;
import fri.music.instrument.notespiano.wave.NotesWithDifferenceToneInversionsPianoPlayer;
import fri.music.instrument.notespiano.wave.NotesWithDifferenceTonePianoPlayer;
import fri.music.instrument.wave.DifferenceToneForIntervalPiano;
import fri.music.instrument.wave.DifferenceToneForNotesPiano;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.instrument.wave.IntervalPlayingPiano;
import fri.music.instrument.wave.TriadPlayingPiano;
import fri.music.instrument.wave.slider.AbstractFrequencySliders;
import fri.music.instrument.wave.slider.FrequencyChordSliders;
import fri.music.instrument.wave.slider.FrequencyDifferenceSliders;
import fri.music.justintonation.swing.CheckLauncher;
import fri.music.swingutils.FrameStarter;
import fri.music.swingutils.layout.SizeUtil;
import fri.music.swingutils.text.HtmlBrowser;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * The differencetones-piano application.
 * It exposes all the apps available in this project.
 * 
 * @author Fritz Ritzberger, Dec 2024 - Oct 2025
 */
public final class Main
{
    /** Application starter.*/
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> 
            FrameStarter.start(
                    "Welcome to the World of Difference-Tones!",
                    new Main().panel,
                    new Dimension(1020, 700))
        );
    }
    
    
    private final JPanel panel = new JPanel(new BorderLayout());
    
    private Main() {
        panel.add(buildHtmlBrowser(), BorderLayout.CENTER);
        panel.add(buildLeftButtons(), BorderLayout.WEST);
        panel.add(buildRightButtons(), BorderLayout.EAST);
    }

    private HtmlBrowser buildHtmlBrowser() {
        final URL url = HtmlResources.class.getResource("introduction.html");
        return new HtmlBrowser(url, HtmlResources.class);
    }

    private JComponent buildLeftButtons() {
        final JToolBar toolbar = new VerticalToolbar();
        toolbar.add(buildNotesWithDifferenceToneInversionsPianoPlayer());
        toolbar.add(buildNotesWithDifferenceTonePianoPlayer());
        toolbar.add(buildNotesPianoPlayer());
        toolbar.add(buildDifferenceToneInversionsPiano());
        toolbar.add(buildDifferenceToneForIntervalPiano());
        return toolbar;
    }

    private JComponent buildRightButtons() {
        final JToolBar toolbar = new VerticalToolbar();
        toolbar.add(buildFrequencyDifferenceSliders());
        toolbar.add(buildFrequencyChordSliders());
        toolbar.add(buildIntervalPlayingPiano());
        toolbar.add(buildTriadPlayingPiano());
        toolbar.add(buildJustIntonationCheckerConfiguration());
        return toolbar;
    }
    
    // left buttons
    
    private int octaves = 7;
    private String lowestToneName = "C2";
    private ToneSystem toneSystem = new EqualTemperament(lowestToneName, octaves);
    
    private record WavePianoParameters(PianoWithSound.Configuration config, WaveSoundChannel soundChannel)
    {
    }
    
    private WavePianoParameters wavePianoParams = new WavePianoParameters(
            new PianoWithSound.Configuration(octaves, lowestToneName),
            new GenericWaveSoundChannel(toneSystem.tones(), null));

    
    private Action buildNotesWithDifferenceToneInversionsPianoPlayer() {
        final String title = HelpForCompose.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final NotesWithDifferenceToneInversionsPianoPlayer player = 
                        new NotesWithDifferenceToneInversionsPianoPlayer(
                            new DifferenceToneInversionsPiano(wavePianoParams.config, wavePianoParams.soundChannel));
                final JComponent playerPanel = player.getPlayer(null);
                FrameStarter.start(title, false, playerPanel, player.getWindowClosingListener(), null);
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }

    private Action buildNotesWithDifferenceTonePianoPlayer() {
        final String title = HelpForAutoCompose.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final NotesWithDifferenceTonePianoPlayer player = new NotesWithDifferenceTonePianoPlayer(
                        new DifferenceToneForNotesPiano(wavePianoParams.config, wavePianoParams.soundChannel));
                final JComponent playerPanel = player.getPlayer(NoteExamples.AUGUSTIN.notes());
                FrameStarter.start(title, false, playerPanel, player.getWindowClosingListener(), null);
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }

    private Action buildNotesPianoPlayer() {
        final String title = HelpForNotes.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final NotesPianoPlayer player = new NotesPianoPlayer(
                        new PianoWithVolume(wavePianoParams.config, wavePianoParams.soundChannel));
                final JComponent playerPanel = player.getPlayer(NoteExamples.ODE_TO_JOY.notes());
                FrameStarter.start(title, false, playerPanel, player.getWindowClosingListener(), null);
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }

    private Action buildDifferenceToneInversionsPiano() {
        final String title = "Find Out Which Intervals Can Generate a Difference-Tone";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final PianoWithSound piano = 
                        new DifferenceToneInversionsPiano(wavePianoParams.config, wavePianoParams.soundChannel);
                final JComponent panel = piano.getKeyboard();
                FrameStarter.start(title, false, panel, piano.getWindowClosingListener(), null);
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }

    private Action buildDifferenceToneForIntervalPiano() {
        final String title = "See and Hear Difference-Tones Generated by Various Intervals";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final PianoWithSound piano = 
                        new DifferenceToneForIntervalPiano(wavePianoParams.config, wavePianoParams.soundChannel);
                final JComponent panel = piano.getKeyboard();
                FrameStarter.start(title, false, panel, piano.getWindowClosingListener(), null);
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }

    // right buttons
    
    private Action buildJustIntonationCheckerConfiguration() {
        final String title = "Check Just-Intonation Tunings for Purity";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                FrameStarter.start(title, false, new CheckLauncher(title).panel);
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }

    private Action buildIntervalPlayingPiano() {
        final String title = "Hear Interval Beatings in Various Tunings";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final PianoWithSound piano = 
                        new IntervalPlayingPiano(wavePianoParams.config, wavePianoParams.soundChannel);
                final JComponent panel = piano.getKeyboard();
                FrameStarter.start(title, false, panel, piano.getWindowClosingListener(), null);
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }

    private Action buildTriadPlayingPiano() {
        final String title = "Hear Triad Beatings in Various Tunings";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final PianoWithSound piano = new TriadPlayingPiano(wavePianoParams.config, wavePianoParams.soundChannel);
                final JComponent panel = piano.getKeyboard();
                FrameStarter.start(title, false, panel, piano.getWindowClosingListener(), null);
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }

    private Action buildFrequencyChordSliders() {
        final String title = "Study Chord Beatings in Various Tunings with Frequency Sliders";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final AbstractFrequencySliders sliders = new FrequencyChordSliders();
                FrameStarter.start(title, false, sliders.panel, sliders.getWindowClosingListener(), new Dimension(1200, 510));
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }

    private Action buildFrequencyDifferenceSliders() {
        final String title = "See Intervals and Their Difference-Tones on Frequency Sliders";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final AbstractFrequencySliders sliders = new FrequencyDifferenceSliders();
                FrameStarter.start(title, false, sliders.panel, sliders.getWindowClosingListener(), new Dimension(1200, 410));
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        return action;
    }


    /** Enables line breaks for long texts on buttons, instead of "...". */
    private String asHtml(String text) {
        return "<html>"+text+"</html>";
    }

    private static class VerticalToolbar extends JToolBar
    {
        public VerticalToolbar() {
            super(JToolBar.VERTICAL);
        }
        
        @Override
        protected JButton createActionComponent(Action action) {
            final JButton button = super.createActionComponent(action);
            
            final Font buttonFont = button.getFont();
            button.setFont(buttonFont.deriveFont(buttonFont.getStyle(), buttonFont.getSize() + 3f));
            SizeUtil.forceSize(button, new Dimension(150, 130));
            return button;
        }
    }
}