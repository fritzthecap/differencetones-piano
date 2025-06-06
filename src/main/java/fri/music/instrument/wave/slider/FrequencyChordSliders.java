package fri.music.instrument.wave.slider;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.ToneSystem;
import fri.music.swingutils.SmartComboBox;
import fri.music.swingutils.SmartSlider;

/**
 * Lets try out 4 frequencies combined.
 */
public class FrequencyChordSliders extends AbstractFrequencySliders
{
    private SliderPanel frequencyPanel1;
    private SliderPanel frequencyPanel2;
    private SliderPanel frequencyPanel3;
    private SliderPanel frequencyPanel4;
    
    /** Render tone range C3 - C8. */
    public FrequencyChordSliders() {
        this(null);
    }
    
    /** Render given tone range. */
    public FrequencyChordSliders(ToneSystem toneSystem) {
        super(toneSystem);
    }
    
    @Override
    protected SliderPanel[] getSliderPanels() {
        return new SliderPanel[] {
            frequencyPanel1,
            frequencyPanel2,
            frequencyPanel3,
            frequencyPanel4
        };
    }
    
    @Override
    protected JPanel[] createFrequencySliderPanels(JSlider amplitudeSlider, JSlider gainSlider, JComboBox<String> waveChoice) {
        frequencyPanel1 = new SliderPanel(tones(), "Frequency", "1", amplitudeSlider, gainSlider, waveChoice);
        frequencyPanel2 = new SliderPanel(tones(), "Frequency", "2", amplitudeSlider, gainSlider, waveChoice);
        frequencyPanel3 = new SliderPanel(tones(), "Frequency", "3", amplitudeSlider, gainSlider, waveChoice);
        frequencyPanel4 = new SliderPanel(tones(), "Frequency", "4", amplitudeSlider, gainSlider, waveChoice);
        
        return new JPanel[] {
            frequencyPanel1.sliderPanel,
            frequencyPanel2.sliderPanel,
            frequencyPanel3.sliderPanel,
            frequencyPanel4.sliderPanel
        };
    }

    /** Creates "Start / Stop All" button and chord selection. */
    @Override
    protected JComponent createInfoPanel() {
        final JButton startStopAll = new JButton(SliderPanel.START_LABEL+" / "+SliderPanel.STOP_LABEL+" All");
        startStopAll.setForeground(Color.BLUE);
        startStopAll.setPreferredSize(SliderPanel.STARTSTOP_BUTTON_SIZE);
        startStopAll.setMaximumSize(SliderPanel.STARTSTOP_BUTTON_SIZE);

        startStopAll.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                boolean stop = false;
                for (SliderPanel sliderPanel : getSliderPanels())
                    if (sliderPanel.isPlaying())
                        stop = true;

                if (stop) {
                    startStopAll.setForeground(Color.BLUE);
                    for (SliderPanel sliderPanel : getSliderPanels())
                        if (sliderPanel.isPlaying())
                            sliderPanel.startStop.doClick();
                }
                else {
                    startStopAll.setForeground(Color.RED);
                    for (SliderPanel sliderPanel : getSliderPanels())
                        if (false == sliderPanel.isPlaying())
                            sliderPanel.startStop.doClick();
                }
            }
        });
        
        final JRadioButton majorChord = new JRadioButton("Major Chord", true);
        final JRadioButton minorChord = new JRadioButton("Minor Chord");
        final JPanel majorChordPanel = createTwoRadioButtons("Mode", majorChord, minorChord);

        final JRadioButton majorSeventh = new JRadioButton("Major 7th");
        final JRadioButton minorSeventh = new JRadioButton("Minor 7th", true);
        final JPanel majorSeventhPanel = createTwoRadioButtons("Seventh", majorSeventh, minorSeventh);
        
        final int lowestOctave = tones().getLowestOctave();
        final int highestOctave = tones().getHighestOctave() - 1; // one octave is needed to build chord
        final JSlider octaves = new SmartSlider(
                lowestOctave, 
                highestOctave,
                Math.max(Math.min(4, highestOctave), lowestOctave));
        octaves.setBorder(BorderFactory.createTitledBorder("Octave"));
        octaves.setSnapToTicks(true);
        octaves.setPaintLabels(true);
        octaves.setPaintTicks(true);
        octaves.setMajorTickSpacing(1);

        final JComboBox<String> baseNoteChoice = new SmartComboBox(ToneSystem.IPN_BASE_NAMES);
        baseNoteChoice.setBorder(BorderFactory.createTitledBorder("Chord"));
        baseNoteChoice.setSelectedItem("C4");
        final ActionListener baseNoteActionListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setChord(
                        (String) baseNoteChoice.getSelectedItem(),
                        octaves.getValue(),
                        majorChord.isSelected(),
                        majorSeventh.isSelected());
                
                for (SliderPanel panel : getSliderPanels()) {
                    if (panel.isPlaying()) { // is running
                        panel.startStop.doClick(); // stop
                        panel.startStop.doClick(); // restart with new chord
                    }
                }
            }
        };
        baseNoteChoice.addActionListener(baseNoteActionListener);
        majorChord.addActionListener(baseNoteActionListener);
        minorChord.addActionListener(baseNoteActionListener);
        majorSeventh.addActionListener(baseNoteActionListener);
        minorSeventh.addActionListener(baseNoteActionListener);
        octaves.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                if (octaves.getValueIsAdjusting() == false)
                    baseNoteActionListener.actionPerformed(null);
            }
        });
        
        // set the chord in sliders to selected base note
        baseNoteActionListener.actionPerformed(null);
        
        final JPanel infoPanel = new JPanel();
        infoPanel.add(Box.createHorizontalStrut(14));
        infoPanel.add(startStopAll);
        infoPanel.add(Box.createHorizontalStrut(24));
        infoPanel.add(baseNoteChoice);
        infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.X_AXIS));
        infoPanel.add(majorChordPanel);
        infoPanel.add(majorSeventhPanel);
        infoPanel.add(octaves);
        
        final JPanel shiftLeftPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        shiftLeftPanel.add(infoPanel);
        
        return shiftLeftPanel;
    }

    private JPanel createTwoRadioButtons(String title, JRadioButton first, JRadioButton second) {
        final ButtonGroup radioGroup = new ButtonGroup(); // exclusive selection
        radioGroup.add(first);
        radioGroup.add(second);
        final JPanel radioPanel = new JPanel();
        radioPanel.setLayout(new BoxLayout(radioPanel, BoxLayout.Y_AXIS));
        radioPanel.add(first);
        radioPanel.add(second);
        radioPanel.setBorder(BorderFactory.createTitledBorder(title));
        return radioPanel;
    }

    private void setChord(String baseNote, int octave, boolean majorChord, boolean majorSeventh) {
        final int thirdSemitones = ToneSystem.semitoneSteps(majorChord ? ToneSystem.MAJOR_THIRD : ToneSystem.MINOR_THIRD);
        final int fifthSemitones = ToneSystem.semitoneSteps(ToneSystem.FIFTH);
        final int seventhSemitones = ToneSystem.semitoneSteps(majorSeventh ? ToneSystem.MAJOR_SEVENTH : ToneSystem.MINOR_SEVENTH);
        final String first = baseNote+octave;
        final int baseIndex = tones().indexOf(first);
        final String third = getIndexInBounds(thirdSemitones, baseIndex);
        final String fifth = getIndexInBounds(fifthSemitones, baseIndex);
        final String seventh = getIndexInBounds(seventhSemitones, baseIndex);
        frequencyPanel1.setNote(first);
        frequencyPanel2.setNote(third);
        frequencyPanel3.setNote(fifth);
        frequencyPanel4.setNote(seventh);
    }

    private String getIndexInBounds(int semitones, int baseIndex) {
        while (baseIndex + semitones >= tones().tones.length)
            baseIndex -= ToneSystem.SEMITONES_PER_OCTAVE;
        return tones().tones[baseIndex + semitones].ipnName;
    }
}