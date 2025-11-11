package fri.music.apps;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;
import java.util.Properties;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import fri.music.HtmlResources;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.instrument.configuration.ConfiguredPianoFactoryLauncher;
import fri.music.instrument.configuration.HelpForTextDisplay;
import fri.music.instrument.configuration.PianoConfigurationPanel;
import fri.music.instrument.configuration.TuningsAndPurityCheckLauncher;
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
import fri.music.instrument.wave.HelpForDifferenceToneForIntervalsPiano;
import fri.music.instrument.wave.HelpForIntervalLists;
import fri.music.instrument.wave.IntervalPlayingPiano;
import fri.music.instrument.wave.TriadPlayingPiano;
import fri.music.instrument.wave.slider.AbstractFrequencySliders;
import fri.music.instrument.wave.slider.FrequencyChordSliders;
import fri.music.instrument.wave.slider.FrequencyDifferenceSliders;
import fri.music.instrument.wave.slider.HelpForFrequencyChordSliders;
import fri.music.instrument.wave.slider.HelpForFrequencyDifferenceSliders;
import fri.music.utils.swing.layout.SizeUtil;
import fri.music.utils.swing.text.HelpWindowSingleton;
import fri.music.utils.swing.text.HtmlBrowser;
import fri.music.utils.swing.window.FrameStarter;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * The differencetones-piano application.
 * This class exposes all the apps available in this project.
 * 
 * @author Fritz Ritzberger, Dec 2024 - Oct 2025
 */
public class DifferenceTonesPiano
{
    private static final int APP_BUTTON_WIDTH = 150; // left and right toolbar widths

    /** Application starter. */
    public static void main(String[] args) {
        final String version = readVersionFromMavenPomProperties();
        SwingUtilities.invokeLater(() -> {
            final JFrame mainFrame = FrameStarter.start(
                    "Welcome to the World of Difference-Tones "+version,
                    new DifferenceTonesPiano().panel,
                    new Dimension(1086, 700));
            // let apps start screen-centered instead of cascaded to mainFrame
            FrameStarter.setNonLayoutRelevant(mainFrame);
        });
    }

    private static String readVersionFromMavenPomProperties() {
        final String path = "/META-INF/maven/fri.music/differencetones-piano/pom.properties";
        final Properties mavenPomProperties = new Properties();
        try {
            mavenPomProperties.load(DifferenceTonesPiano.class.getResourceAsStream(path));
            return mavenPomProperties.getProperty("version");
        }
        catch (Throwable e) {
            e.printStackTrace();
            return "!";
        }
    }

    private final JPanel panel = new JPanel(new BorderLayout());

    private DifferenceTonesPiano() {
        panel.add(buildHtmlBrowser(), BorderLayout.CENTER);
        panel.add(buildLeftButtons(), BorderLayout.WEST);
        panel.add(buildRightButtons(), BorderLayout.EAST);
    }

    private HtmlBrowser buildHtmlBrowser() {
        final URL url = HtmlResources.class.getResource("introduction.html");
        return new HtmlBrowser(url);
    }

    private JComponent buildLeftButtons() {
        final JToolBar toolbar = new VerticalToolbar();
        toolbar.add(buildConfigurePianoButton());
        toolbar.add(buildDifferenceToneForIntervalPiano());
        toolbar.add(buildDifferenceToneInversionsPiano());
        toolbar.add(buildNotesWithDifferenceTonePianoPlayer());
        toolbar.add(buildNotesWithDifferenceToneInversionsPianoPlayer());
        return toolbar;
    }

    private JComponent buildRightButtons() {
        final JToolBar toolbar = new VerticalToolbar();
        toolbar.add(buildQuickTourButton());
        toolbar.add(buildFrequencyDifferenceSliders());
        toolbar.add(buildFrequencyChordSliders());
        toolbar.add(buildTuningDisplayAndPurityChecker());
        toolbar.add(buildMoreButton());
        return toolbar;
    }
    
    
    private record WavePianoParameters(PianoWithSound.Configuration pianoConfiguration, WaveSoundChannel soundChannel)
    {
    }
    
    private WavePianoParameters wavePianoParameters = new WavePianoParameters(
            new PianoWithSound.Configuration(7, "C2"),
            new GenericWaveSoundChannel(null, null));
    
    
    // left buttons
    
    private JButton buildConfigurePianoButton() {
        final JButton configurePianoButton = new JButton("Configure Your Piano");
        configurePianoButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final JPanel configurePianoPanel = new JPanel(new BorderLayout());
                final PianoConfigurationPanel pianoConfigurationPanel = 
                        new PianoConfigurationPanel(wavePianoParameters.pianoConfiguration());
                configurePianoPanel.add(pianoConfigurationPanel.panel, BorderLayout.CENTER);
                final int answer = JOptionPane.showConfirmDialog(
                        panel, 
                        configurePianoPanel,
                        "Piano Configuration", 
                        JOptionPane.OK_CANCEL_OPTION,
                        JOptionPane.PLAIN_MESSAGE);
                
                if (answer == JOptionPane.OK_OPTION) {
                    final PianoWithSound.Configuration newConfiguration = pianoConfigurationPanel.getPianoConfiguration();
                    wavePianoParameters = new WavePianoParameters(newConfiguration, wavePianoParameters.soundChannel());
                }
            }
        });
        return configureTopButton(configurePianoButton, "Choose Tone Range and More for Your Piano");
    }

    private Action buildDifferenceToneForIntervalPiano() {
        final String title = HelpForDifferenceToneForIntervalsPiano.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final DifferenceToneForIntervalPiano piano = 
                        new DifferenceToneForIntervalPiano(wavePianoParameters.pianoConfiguration, wavePianoParameters.soundChannel);
                final JComponent keyboardPanel = piano.getKeyboard();
                FrameStarter.start(title, false, keyboardPanel, piano.getWindowClosingListener());
            }
        };
        return setActionNameAsHtml(title, action);
    }

    private Action buildDifferenceToneInversionsPiano() {
        final String title = HelpForIntervalLists.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final DifferenceToneInversionsPiano piano = 
                        new DifferenceToneInversionsPiano(wavePianoParameters.pianoConfiguration, wavePianoParameters.soundChannel);
                final JComponent panel = piano.getKeyboard();
                FrameStarter.start(title, false, panel, piano.getWindowClosingListener());
            }
        };
        return setActionNameAsHtml(title, action);
    }
    
    private Action buildNotesWithDifferenceTonePianoPlayer() {
        final String title = HelpForAutoCompose.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final NotesWithDifferenceTonePianoPlayer player = new NotesWithDifferenceTonePianoPlayer(
                        new DifferenceToneForNotesPiano(wavePianoParameters.pianoConfiguration, wavePianoParameters.soundChannel));
                final JComponent playerPanel = player.getPlayer(NoteExamples.AUGUSTIN.notes());
                FrameStarter.start(title, false, playerPanel, player.getWindowClosingListener());
            }
        };
        return setActionNameAsHtml(title, action);
    }

    private Action buildNotesWithDifferenceToneInversionsPianoPlayer() {
        final String title = HelpForCompose.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final NotesWithDifferenceToneInversionsPianoPlayer player = 
                        new NotesWithDifferenceToneInversionsPianoPlayer(
                            new DifferenceToneInversionsPiano(wavePianoParameters.pianoConfiguration, wavePianoParameters.soundChannel));
                final JComponent playerPanel = player.getPlayer(null);
                FrameStarter.start(title, false, playerPanel, player.getWindowClosingListener());
            }
        };
        return setActionNameAsHtml(title, action);
    }

    // right buttons
    
    private JButton buildQuickTourButton() {
        final JButton quickTourButton = new JButton("Quick Tour");
        quickTourButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                HelpWindowSingleton.start(panel, "Quick Tour", HtmlResources.class.getResource("quicktour.html"));
            }
        });
        return configureTopButton(quickTourButton, "Brief Description of Two Functionalities of the Application");
    }

    private Action buildFrequencyDifferenceSliders() {
        final String title = HelpForFrequencyDifferenceSliders.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final AbstractFrequencySliders sliders = new FrequencyDifferenceSliders();
                FrameStarter.start(title, false, sliders.panel, sliders.getWindowClosingListener(), new Dimension(1200, 410));
            }
        };
        return setActionNameAsHtml(title, action);
    }

    private Action buildFrequencyChordSliders() {
        final String title = HelpForFrequencyChordSliders.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final AbstractFrequencySliders sliders = new FrequencyChordSliders();
                FrameStarter.start(title, false, sliders.panel, sliders.getWindowClosingListener(), new Dimension(1200, 510));
            }
        };
        return setActionNameAsHtml(title, action);
    }

    private JFrame tuningsAndPurityCheckerFrame; // instance-singleton
    
    private Action buildTuningDisplayAndPurityChecker() {
        final String title = HelpForTextDisplay.TITLE;
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (tuningsAndPurityCheckerFrame == null)
                    tuningsAndPurityCheckerFrame = FrameStarter.start(
                            title, 
                            new TuningsAndPurityCheckLauncher().panel);
                else
                    tuningsAndPurityCheckerFrame.setVisible(true);
            }
        };
        return setActionNameAsHtml(title, action);
    }

    private JFrame moreLauncherFrame;
    
    private Action buildMoreButton() {
        final String title = "More ....";
        final Action action = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (moreLauncherFrame == null)
                    moreLauncherFrame = FrameStarter.start(title, new MoreLaunchers());
                else
                    moreLauncherFrame.setVisible(true);
            }
        };
        return setActionNameAsHtml(title, action);
    }

    
    
    private Action setActionNameAsHtml(String title, Action action) {
        action.putValue(Action.NAME, "<html>"+title+"</html>");
        return action;
    }
    
    private JButton configureTopButton(JButton button, String tooltip) {
        button.setToolTipText(tooltip);
        
        button.setBackground(Color.WHITE);
        button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        button.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        
        return SizeUtil.forceSize(button, new Dimension(APP_BUTTON_WIDTH, 40));
    }

    private JButton configureAppLauncherButton(JButton button) {
        button.setToolTipText("Click to Launch App");
        
        final Font buttonFont = button.getFont();
        button.setFont(buttonFont.deriveFont(buttonFont.getStyle(), buttonFont.getSize() + 3f));
        button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        button.setAlignmentX(JComponent.CENTER_ALIGNMENT);
        
        return SizeUtil.forceSize(button, new Dimension(APP_BUTTON_WIDTH, 130));
    }
    
    
    
    private class VerticalToolbar extends JToolBar
    {
        public VerticalToolbar() {
            super(JToolBar.VERTICAL);
        }
        
        @Override
        protected JButton createActionComponent(Action action) {
            return configureAppLauncherButton(super.createActionComponent(action));
        }
    }
    
    
    
    private class MoreLaunchers extends JPanel
    {
        public MoreLaunchers() {
            super(new FlowLayout());
            
            launcher(buildIntervalPlayingPiano());
            launcher(buildTriadPlayingPiano());
            launcher(buildNotesPianoPlayer());
            launcher(buildConfiguredPianoFactory());
        }
        
        private void launcher(Action action) {
            add(configureAppLauncherButton(new JButton(action)));
        }


        private Action buildNotesPianoPlayer() {
            final String title = HelpForNotes.TITLE;
            final Action action = new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    final NotesPianoPlayer player = new NotesPianoPlayer(
                            new PianoWithVolume(wavePianoParameters.pianoConfiguration, wavePianoParameters.soundChannel));
                    final JComponent playerPanel = player.getPlayer(NoteExamples.ODE_TO_JOY.notes());
                    FrameStarter.start(moreLauncherFrame, title, playerPanel, player.getWindowClosingListener());
                }
            };
            return setActionNameAsHtml(title, action);
        }
        
        private Action buildIntervalPlayingPiano() {
            final String title = "Hear Interval Beatings in Various Tunings";
            final Action action = new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    final PianoWithSound piano = 
                            new IntervalPlayingPiano(wavePianoParameters.pianoConfiguration, wavePianoParameters.soundChannel);
                    final JComponent panel = piano.getKeyboard();
                    FrameStarter.start(moreLauncherFrame, title, panel, piano.getWindowClosingListener());
                }
            };
            return setActionNameAsHtml(title, action);
        }
        
        private Action buildTriadPlayingPiano() {
            final String title = "Hear Triad Beatings in Various Tunings";
            final Action action = new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    final PianoWithSound piano = new TriadPlayingPiano(wavePianoParameters.pianoConfiguration, wavePianoParameters.soundChannel);
                    final JComponent keyboardPanel = piano.getKeyboard();
                    FrameStarter.start(moreLauncherFrame, title, keyboardPanel, piano.getWindowClosingListener());
                }
            };
            return setActionNameAsHtml(title, action);
        }
        
        private JFrame configuredPianoFactoryFrame;
        
        private Action buildConfiguredPianoFactory() {
            final String title = "Configure Pianos (API Showcase)";
            final Action action = new AbstractAction() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (configuredPianoFactoryFrame == null)
                        configuredPianoFactoryFrame = new ConfiguredPianoFactoryLauncher(moreLauncherFrame).frame;
                    else
                        configuredPianoFactoryFrame.setVisible(true);
                }
            };
            return setActionNameAsHtml(title, action);
        }
    }
}