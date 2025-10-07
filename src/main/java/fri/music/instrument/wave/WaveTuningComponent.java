package fri.music.instrument.wave;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JComboBox;
import javax.swing.SwingUtilities;
import fri.music.ToneSystem;
import fri.music.instrument.TuningComponent;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Lets change tunings on a wave-soundchannel, or notifies a listener.
 */
public class WaveTuningComponent extends TuningComponent
{
    /** 
     * If no sound-channel is given to constructor, 
     * this listener will be notified on any tuning change.
     * It is possible to use both listener and sound-channel.
     */
    public interface Listener
    {
        void tuningChanged(ToneSystem toneSystem);
    }
    
    private final WaveSoundChannel soundChannel;
    private final Listener listener;
    
    public WaveTuningComponent(String lowestToneIpnName, int octaves, WaveSoundChannel soundChannel) {
        this(lowestToneIpnName, octaves, soundChannel, null);
    }
    
    public WaveTuningComponent(String lowestToneIpnName, int octaves, WaveSoundChannel soundChannel, Listener listener) {
        super(lowestToneIpnName, octaves);
        
        if (soundChannel == null && listener == null)
            throw new IllegalArgumentException("Need either sound-channel or tuning-listener!");
        
        this.soundChannel = soundChannel;
        this.listener = listener;
    }
    
    @Override
    protected void addListeners(final JComboBox<String> tuningChoice, boolean initialTuningWasSet) {
        final ActionListener actionListener = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final ToneSystem toneSystem = getTuning();
                
                if (soundChannel != null)
                    soundChannel.setTones(toneSystem.tones());
                
                if (listener != null)
                    listener.tuningChanged(toneSystem);
            }
        };
        tuningChoice.addActionListener(actionListener);
        
        if (initialTuningWasSet)
            SwingUtilities.invokeLater(() -> actionListener.actionPerformed(null)); // let callers initialize first
    }
}