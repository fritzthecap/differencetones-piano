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
import fri.music.instrument.notespiano.wave.NotesWithDifferenceToneInversionsPianoPlayer;
import fri.music.instrument.wave.DifferenceToneInversionsPiano;
import fri.music.instrument.wave.slider.AbstractFrequencySliders;
import fri.music.instrument.wave.slider.FrequencyChordSliders;
import fri.music.instrument.wave.slider.FrequencyDifferenceSliders;
import fri.music.swingutils.FrameStarter;
import fri.music.swingutils.layout.SizeUtil;
import fri.music.swingutils.text.HtmlBrowser;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * The differencetone-piano application.
 * 
 * @author Fritz Ritzberger, Dec 2024 - Oct 2025
 */
public final class Main
{
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> 
            FrameStarter.start(
                    "Welcome to the World of Difference-Tones!",
                    new Main().panel,
                    new Dimension(1000, 700))
        );
    }
    
    private final JPanel panel = new JPanel(new BorderLayout());
    
    private Main() {
        final URL url = HtmlResources.class.getResource("introduction.html");
        final HtmlBrowser htmlBrowser = new HtmlBrowser(url, HtmlResources.class);
        panel.add(htmlBrowser, BorderLayout.CENTER);
        panel.add(buildLeftButtons(), BorderLayout.WEST);
        panel.add(buildRightButtons(), BorderLayout.EAST);
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
            button.setFont(buttonFont.deriveFont(buttonFont.getStyle(), buttonFont.getSize() + 2f));
            SizeUtil.forceSize(button, new Dimension(140, 120));
            return button;
        }
    }

    private JComponent buildLeftButtons() {
        final JToolBar toolbar = new VerticalToolbar();
        toolbar.add(buildDifferenceToneForIntervalPiano());
        toolbar.add(buildDifferenceToneInversionsPiano());
        toolbar.add(buildNotesPianoPlayer());
        toolbar.add(buildNotesWithDifferenceTonePianoPlayer());
        toolbar.add(buildNotesWithDifferenceToneInversionsPianoPlayer());
        return toolbar;
    }

    private JComponent buildRightButtons() {
        final JToolBar toolbar = new VerticalToolbar();
        toolbar.add(buildFrequencyDifferenceSliders());
        toolbar.add(buildFrequencyChordSliders());
        toolbar.add(buildTriadPlayingPiano());
        toolbar.add(buildJustIntonationCheckerConfiguration());
        toolbar.add(buildPianoConfiguration());
        return toolbar;
    }
    
    /** Enables line breaks for long texts on buttons, instead of "...". */
    private String asHtml(String text) {
        return "<html>"+text+"</html>";
    }

    // left buttons
    
    private Action buildNotesWithDifferenceToneInversionsPianoPlayer() {
        final String title = "Compose Difference-Tone Intervals for a Melody";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final PianoWithSound.Configuration config = new PianoWithSound.Configuration(7, "C2");
                final WaveSoundChannel soundChannel = new GenericWaveSoundChannel(null, null);
                final NotesWithDifferenceToneInversionsPianoPlayer player = 
                        new NotesWithDifferenceToneInversionsPianoPlayer(
                                new DifferenceToneInversionsPiano(config, soundChannel));
                final JComponent playerPanel = player.getPlayer(null);
                FrameStarter.start(title, false, playerPanel, player.getWindowClosingListener(), null);
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }

    private Action buildNotesWithDifferenceTonePianoPlayer() {
        final String title = "Hear Auto-Composed Difference-Tone Intervals for Written Notes";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                throw new RuntimeException("Implement me!");
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }

    private Action buildNotesPianoPlayer() {
        final String title = "Record Notes with Piano and Play Them";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                throw new RuntimeException("Implement me!");
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }

    private Action buildDifferenceToneInversionsPiano() {
        final String title = "See Which Intervals Can Generate a Difference-Tone";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                throw new RuntimeException("Implement me!");
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }

    private Action buildDifferenceToneForIntervalPiano() {
        final String title = "Hear and See Difference Tones Generated by Various Intervals";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                throw new RuntimeException("Implement me!");
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }

    // right buttons
    
    private Action buildJustIntonationCheckerConfiguration() {
        final String title = "Check Just-Intonation Tunings for Purity of Intervals and Triads";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                throw new RuntimeException("Implement me!");
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }

    private Action buildPianoConfiguration() {
        final String title = "Configure Various Pianos (API Demo)";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                throw new RuntimeException("Implement me!");
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }

    private Action buildTriadPlayingPiano() {
        final String title = "Hear Triad Beatings in Different Tunings";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                throw new RuntimeException("Implement me!");
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }

    private Action buildFrequencyChordSliders() {
        final String title = "Study Chord Beatings in Different Tunings with Frequency Sliders";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final AbstractFrequencySliders sliders = new FrequencyChordSliders();
                FrameStarter.start(title, false, sliders.panel, sliders.getWindowClosingListener(), new Dimension(1200, 510));
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }

    private Action buildFrequencyDifferenceSliders() {
        final String title = "See Intervals and Their Difference Tones on Frequency Sliders";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final AbstractFrequencySliders sliders = new FrequencyDifferenceSliders();
                FrameStarter.start(title, false, sliders.panel, sliders.getWindowClosingListener(), new Dimension(1200, 410));
            }
        };
        action.putValue(Action.NAME, asHtml(title));
        action.putValue(Action.SHORT_DESCRIPTION, title);
        return action;
    }
}