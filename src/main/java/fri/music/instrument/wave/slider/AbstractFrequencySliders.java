package fri.music.instrument.wave.slider;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.ArrayList;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.JustIntonation;
import fri.music.ToneSystem;
import fri.music.Tones;
import fri.music.instrument.swing.SmartComboBox;
import fri.music.instrument.wave.TuningComponent;
import fri.music.wavegenerator.SineWaveGenerator;
import fri.music.wavegenerator.WaveGenerator;

/**
 * Common functionality needed to expose several frequency sliders.
 * This includes amplitude-, gain- and wave-controls.
 */
public abstract class AbstractFrequencySliders
{
    public static final String RECOMMENDED_LOWEST = "C3";
    public static final int RECOMMENDED_OCTAVES = 5;
    
    /** Contains all of the UI. Add this to some parent UI. */
    public final JComponent panel;
    
    private Tones tones;

    /** Render given tone range, or RECOMMENDED tone range C3 - C8 when null. */
    protected AbstractFrequencySliders(ToneSystem toneSystem) {
        if (toneSystem == null)
            toneSystem = new JustIntonation(RECOMMENDED_LOWEST, RECOMMENDED_OCTAVES);
        initializeTones(toneSystem);
        
        final JSlider amplitudeSlider = createAmplitudeSlider();
        final JSlider gainSlider = createGainSlider();
        final JComboBox<String> waveChoice = createWaveChoice();
        final JComboBox<String> tuningChoice = createTuningChoice(toneSystem, amplitudeSlider, gainSlider, waveChoice);
        
        final JPanel[] frequencySliderPanels = createFrequencySliderPanels(amplitudeSlider, gainSlider, waveChoice);
        
        final JComponent infoPanel = createInfoPanel();
        
        panel = new JScrollPane(
            layoutMainPanel(
                amplitudeSlider, 
                gainSlider, 
                waveChoice, 
                tuningChoice, 
                infoPanel,
                frequencySliderPanels
            )
        );
    }
    
    /**
     * Called from constructor. To be overridden for additional initializations.
     * @param toneSystem the tones and frequencies to render in sliders.
     */
    protected void initializeTones(ToneSystem toneSystem) {
        tones = new Tones(toneSystem.tones());
    }
    
    /** @return the currently rendered tones. */
    protected final Tones tones() {
        return tones;
    }

    /**
     * Called from constructor, creates the frequency sliders this application supports.
     * @param amplitudeSlider the amplitude control to listen to.
     * @param gainSlider the gain control to listen to.
     * @param waveChoice the wave-chooser to listen to.
     * @return a panel containing all frequency sliders this application supports.
     */
    protected abstract JPanel[] createFrequencySliderPanels(JSlider amplitudeSlider, JSlider gainSlider, JComboBox<String> waveChoice);
    
    /** @return the frequency sliders this application supports. */
    protected abstract SliderPanel[] getSliderPanels();
    
    /** @return an optional info-panel that will be shown above frequency sliders. */
    protected JComponent createInfoPanel() {
        return null;
    }
    
    /**
     * Called when user chooses a new tuning.
     * Fetch state from sliders, remove them create new ones, set their state, add them.
     * @param amplitudeSlider the amplitude control to listen to.
     * @param gainSlider the gain control to listen to.
     * @param waveChoice the wave-chooser to listen to.
     */
    //protected abstract void recreateFrequencySliderPanels(JSlider amplitudeSlider, JSlider gainSlider, JComboBox<String> waveChoice);
    protected final void recreateFrequencySliderPanels(JSlider amplitudeSlider, JSlider gainSlider, JComboBox<String> waveChoice) {
        beforeRecreateFrequencySliderPanels();
        
        final SliderPanel[] oldSliderPanels = getSliderPanels();
        final Container parent = oldSliderPanels[0].sliderPanel.getParent();
        
        // collect old states
        final List<Boolean> wasPlaying = new ArrayList<>();
        final List<Double> oldFrequency = new ArrayList<>();
        
        for (SliderPanel oldSliderPanel : oldSliderPanels) {
            wasPlaying.add(oldSliderPanel.isPlaying());
            oldFrequency.add(oldSliderPanel.getValue());
            
            oldSliderPanel.close(); // could be playing
            parent.remove(oldSliderPanel.sliderPanel);
        }
        
        createFrequencySliderPanels(amplitudeSlider, gainSlider, waveChoice);
        
        final SliderPanel[] newSliderPanels = getSliderPanels();
        
        // restore old states
        for (int i = 0; i < newSliderPanels.length; i++) {
            final SliderPanel newSliderPanel = newSliderPanels[i];
            parent.add(newSliderPanel.sliderPanel);
            if (shouldRestoreSliderState(newSliderPanel))
                newSliderPanel.setValue(oldFrequency.get(i));
        }
        
        afterRecreateFrequencySliderPanels();
        
        // restore sound
        for (int i = 0; i < newSliderPanels.length; i++) {
            final SliderPanel newSliderPanel = newSliderPanels[i];
            if (shouldRestoreSliderState(newSliderPanel) && wasPlaying.get(i))
                newSliderPanel.startStop.doClick();
        }
        
        parent.revalidate(); // repaints UI
    }
    
    /** Does nothing, called from recreateFrequencySliderPanels() at beginning. */
    protected void beforeRecreateFrequencySliderPanels() {
    }
    
    /** @return true, called from recreateFrequencySliderPanels() on restoring slider state, to be overridden. */
    protected boolean shouldRestoreSliderState(SliderPanel newSliderPanel) {
        return true;
    }

    /** Does nothing, called from recreateFrequencySliderPanels() at end. */
    protected void afterRecreateFrequencySliderPanels() {
    }
    

    /** @return a window-listener to be used for a JFrame showing this panel. */
    public final WindowListener getWindowClosingListener() {
        return new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                close();
            }
        };
    }

    /** Ends playing sounds and releases resources. */
    public final void close() {
        for (SliderPanel sliderPanel : getSliderPanels())
            sliderPanel.close();
    }


    private JSlider createAmplitudeSlider() {
        final JSlider amplitudeSlider = new JSlider();
        amplitudeSlider.setPaintLabels(true);
        amplitudeSlider.setPaintTicks(true);
        amplitudeSlider.setMajorTickSpacing(10);
        amplitudeSlider.setMinimum(0);
        amplitudeSlider.setMaximum(127);
        amplitudeSlider.setValue(7);
        final ChangeListener changeListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                amplitudeSlider.setBorder(BorderFactory.createTitledBorder("Amplitude: "+amplitudeSlider.getValue()));
            }
        };
        amplitudeSlider.addChangeListener(changeListener);
        changeListener.stateChanged(null);
        return amplitudeSlider;
    }

    private JSlider createGainSlider() {
        final JSlider gainSlider = new JSlider();
        final WaveGenerator helper = new SineWaveGenerator();
        gainSlider.setPaintLabels(true);
        gainSlider.setPaintTicks(true);
        gainSlider.setMajorTickSpacing(5);
        gainSlider.setMinimum((int) helper.getMinimumGain());
        gainSlider.setMaximum((int) helper.getMaximumGain());
        gainSlider.setValue((int) helper.getGain());
        gainSlider.setPreferredSize(new Dimension(400, 60));
        final ChangeListener changeListener = new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                gainSlider.setBorder(BorderFactory.createTitledBorder("Gain (Decibels): "+gainSlider.getValue()));
            }
        };
        gainSlider.addChangeListener(changeListener);
        changeListener.stateChanged(null);
        return gainSlider;
    }

    private JComboBox<String> createWaveChoice() {
        final String[] items = new String[] { "Sine", /*"Sawtooth", "Square",*/ "Triangle" };
        final JComboBox<String> waveChoice = new SmartComboBox(items);
        waveChoice.setBorder(BorderFactory.createTitledBorder("Wave Form"));
        waveChoice.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                for (SliderPanel panel : getSliderPanels()) {
                    if (panel.isPlaying()) { // is running
                        panel.startStop.doClick(); // stop
                        panel.startStop.doClick(); // restart with new wave form
                    }
                }
            }
        });
        return waveChoice;
    }

    private JComboBox<String> createTuningChoice(
            final ToneSystem toneSystem,
            final JSlider amplitudeSlider, 
            final JSlider gainSlider, 
            final JComboBox<String> waveChoice)
    {
        final TuningComponent.Listener listener = new TuningComponent.Listener() {
            @Override
            public void tuningChanged(ToneSystem toneSystem) {
                initializeTones(toneSystem);
                recreateFrequencySliderPanels(amplitudeSlider, gainSlider, waveChoice);
            }
        };
        final TuningComponent tuning = new TuningComponent(
                tones().getLowest().ipnName, 
                tones().getOctaves(), 
                null, 
                listener);
        return tuning.getTuningChoice(toneSystem);
    }

    
    private JComponent layoutMainPanel(
            JSlider amplitudeSlider, 
            JSlider gainSlider, 
            JComboBox<String> waveChoice, 
            JComboBox<String> tuningChoice, 
            JComponent infoPanel,
            JPanel[] frequencyPanels)
    {
        final JPanel frequenciesPanel = new JPanel();
        frequenciesPanel.setBorder(BorderFactory.createCompoundBorder(
                        BorderFactory.createEmptyBorder(10, 10, 10, 10),
                        BorderFactory.createRaisedSoftBevelBorder()));
        frequenciesPanel.setLayout(new BoxLayout(frequenciesPanel, BoxLayout.Y_AXIS));
        
        for (JPanel frequencyPanel : frequencyPanels)
            frequenciesPanel.add(frequencyPanel);
        
        final JPanel settingsPanel = new JPanel();
        settingsPanel.setLayout(new BoxLayout(settingsPanel, BoxLayout.X_AXIS));
        settingsPanel.add(tuningChoice);
        settingsPanel.add(waveChoice);
        settingsPanel.add(amplitudeSlider);
        settingsPanel.add(gainSlider);

        final JPanel infoAndSettingsPanel = new JPanel();
        infoAndSettingsPanel.setLayout(new BoxLayout(infoAndSettingsPanel, BoxLayout.Y_AXIS));
        infoAndSettingsPanel.add(settingsPanel);
        if (infoPanel != null)
            infoAndSettingsPanel.add(infoPanel);
        
        final JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.add(infoAndSettingsPanel);
        mainPanel.add(frequenciesPanel);
        mainPanel.add(Box.createVerticalGlue()); // TODO: doesn't work, frequency-panels still stretch!
        
        return mainPanel;
    }
}