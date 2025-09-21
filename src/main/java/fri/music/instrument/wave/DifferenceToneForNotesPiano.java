package fri.music.instrument.wave;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.SequencedSet;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JToolBar;
import javax.swing.border.Border;
import fri.music.differencetones.DifferenceTones;
import fri.music.instrument.PianoWithSound;
import fri.music.instrument.PianoWithVolume;
import fri.music.swingutils.ButtonUtil;
import fri.music.swingutils.DialogUtil;
import fri.music.wavegenerator.GenericWaveSoundChannel;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Piano that marks a difference-tone when two (high) notes are played together.
 * The "Difference Tone" state is rendered by a red border combined with visual selection.
 * <p/>
 * As it is not possible to select two keys with a single mouse-buttons, you will need a
 * <code>PianoKeyConnector</code> and possibly a <code>Player</code> thread to drive this piano.
 */
public class DifferenceToneForNotesPiano extends PianoWithVolume
{
    private JComponent pianoPanel;
    private TuningComponent tuningComponent;
    private DeviationComponent deviationComponent;
    private IntervalRangeComponent intervalRange;

    public DifferenceToneForNotesPiano(PianoWithSound.Configuration config, WaveSoundChannel soundChannel) {
        super(config, soundChannel);
    }
    
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel;

        final JComponent pianoPanel = super.getKeyboard();
        
        int index = 1;
        
        this.tuningComponent = newTuningComponent(config.lowestToneIpnName, config.octaves, getWaveSoundChannel());
        getControlPanel().add(tuningComponent.getChoice(null), index++);
        
        this.deviationComponent = newDeviationComponent(DifferenceTones.DEFAULT_DEVIATION, config.isVertical);
        getControlPanel().add(deviationComponent.getSlider(), index++);
        
        this.intervalRange = new IntervalRangeComponent(null, null);
        getControlPanel().add(intervalRange.getNarrowestChoice(), index++);
        getControlPanel().add(intervalRange.getWidestChoice(), index++);
        
        if (getWaveSoundChannel() instanceof GenericWaveSoundChannel) {
            final WaveChoiceComponent waveChoice = new WaveChoiceComponent((GenericWaveSoundChannel) getWaveSoundChannel());
            getControlPanel().add(waveChoice.choice, index++);
        }
        
        final JButton help = new JButton("Help");
        help.setToolTipText("Piano Settings Description");
        help.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DialogUtil.showModelessHtmlDialog(
                        "Piano Settings", 
                        help, 
                        HelpForPianoSettings.URL,
                        null);
            }
        });
        getControlPanel().add(Box.createHorizontalGlue(), -1);
        final JToolBar toolbar = new JToolBar(); // configure same look&feel as other Help buttons in toolbars
        toolbar.setFloatable(false);
        toolbar.setBorder(null);
        toolbar.add(help);
        getControlPanel().add(toolbar);
        
        return this.pianoPanel = pianoPanel;
    }
    
    /** To be overridden. */
    protected TuningComponent newTuningComponent(String lowestToneIpnName, int octaves, WaveSoundChannel soundChannel) {
        return new TuningComponent(
                lowestToneIpnName, 
                octaves, 
                soundChannel);
    }

    /** To be overridden. */
    protected DeviationComponent newDeviationComponent(double deviation, boolean isVertical) {
        return new DeviationComponent(deviation, isVertical);
    }

    /** Listen to changes in min/max interval range. */
    public void addIntervalRangeActionListener(ActionListener intervalRangeListener) {
        intervalRange.getNarrowestChoice().addActionListener(intervalRangeListener);
        intervalRange.getWidestChoice().addActionListener(intervalRangeListener);
    }
    
    /** @return the used WaveSoundChannel, holding tones of a tone-system. */
    public WaveSoundChannel getWaveSoundChannel() {
        return (WaveSoundChannel) getSoundChannel();
    }

    public String getSelectedTuning() {
        return (String) tuningComponent.getChoice(null).getSelectedItem();
    }
    
    public void setTuningControlsEnabled(boolean enable) {
        tuningComponent.getChoice(null).setEnabled(enable);
        setDifferenceToneParametersEnabled(enable);
    }
    
    public void setDifferenceToneParametersEnabled(boolean enable) {
        deviationComponent.getSlider().setEnabled(enable);
        intervalRange.getNarrowestChoice().setEnabled(enable);
        intervalRange.getWidestChoice().setEnabled(enable);
    }
    
    /** @return the current deviation value from slider as floating-point number 0..1. */
    public double getDeviation() {
        return deviationComponent.getDeviation();
    }
    
    /** @return the minimum value of interval-range for a difference-tone. */
    public String narrowestAllowedInterval() {
        return intervalRange.narrowestAllowedInterval();
    }
    
    /** @return the maximum value of interval-range for a difference-tone. */
    public String widestAllowedInterval() {
        return intervalRange.widestAllowedInterval();
    }
    
    
    /** Overridden to return a DifferenceToneMouseHandler. */
    @Override
    protected MouseHandler newMouseHandler() {
        return new DifferenceToneMouseHandler(this, getWaveSoundChannel());
    }
    
    
    /** When two notes are played together (interval), this listener displays their difference-tone red-bordered. */
    protected static class DifferenceToneMouseHandler extends MouseHandler
    {
        private final WaveSoundChannel soundChannel;
        private final SequencedSet<Keyboard.Key> playingNotes = new LinkedHashSet<>();
        private final Border redBorder;
        private Border originalBorder;
        private Keyboard.Key selectedDifferenceKey;
        
        public DifferenceToneMouseHandler(DifferenceToneForNotesPiano piano, WaveSoundChannel soundChannel) {
            super(piano);
            this.soundChannel = soundChannel;
            this.redBorder = BorderFactory.createLineBorder(Color.RED, 2);
        }
        
        @Override
        protected void noteOn(Keyboard.Key key) {
            super.noteOn(key);
            playingNotes.add(key);
            handleDifferenceKey();
        }

        @Override
        protected void noteOff(Keyboard.Key key) {
            super.noteOff(key);
            playingNotes.remove(key);
            handleDifferenceKey();
        }
        
        private void handleDifferenceKey() {
            final Keyboard.Key[] twoPlayingKeys = getPlayingKeyTuple();
            
            if (twoPlayingKeys != null) { // try to calculate difference tone of the two
                if (selectedDifferenceKey != null) // remove currently selected
                    selectDifferenceTone(selectedDifferenceKey, false);
                
                final Keyboard.Key differenceToneKey = DifferenceToneUtil.getDifferenceToneKey(
                        soundChannel.getTones(), 
                        getPiano().getDeviation(),
                        getPiano(),
                        twoPlayingKeys[0].midiNoteNumber,
                        twoPlayingKeys[1].midiNoteNumber);
                
                if (differenceToneKey != null) {
                    selectedDifferenceKey = differenceToneKey;
                    selectDifferenceTone(selectedDifferenceKey, true); // select and add red border
                }
            }
            else if (selectedDifferenceKey != null) {
                selectDifferenceTone(selectedDifferenceKey, false); // de-select and remove red border
                selectedDifferenceKey = null;
            }
        }
        
        private Keyboard.Key[] getPlayingKeyTuple() {
            if (playingNotes.size() == 2) {
                final Iterator<Keyboard.Key> iterator = playingNotes.iterator();
                final Keyboard.Key key1 = iterator.next();
                final Keyboard.Key key2 = iterator.next();
                return new Keyboard.Key[] { key1, key2 };
            }
            return null;
        }
        
        private void selectDifferenceTone(Keyboard.Key key, boolean select) {
            setRedBorder(key, select);
            ButtonUtil.visualSelect(key, select);
            key.paintImmediately(key.getVisibleRect());
        }
        
        private void setRedBorder(Keyboard.Key key, boolean select) {
            if (originalBorder == null) // save original border for reset
                originalBorder = key.getBorder();
            key.setBorder(select ? redBorder : originalBorder);
        }
        
        private DifferenceToneForNotesPiano getPiano() {
            return (DifferenceToneForNotesPiano) piano;
        }
    }
}