package fri.music.instrument;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSlider;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import fri.music.SoundChannel;
import fri.music.wavegenerator.WaveSoundChannel;

/**
 * Piano that has a volume and a touch-force (velocity) control.
 */
public class PianoWithVolume extends PianoWithSound
{
    private int volume = SoundChannel.MAXIMUM_VOLUME / 8;
    private int velocity = SoundChannel.MAXIMUM_VOLUME / 8;
    
    private int savedVolume = 0;
    private int savedVelocity = 0;
    
    private JComponent pianoPanel;
    private JPanel controlPanel;
    private JSlider volumeSlider;
    private JSlider velocitySlider;
    
    public PianoWithVolume(SoundChannel channel) {
        this(null, channel);
    }
    public PianoWithVolume(PianoWithSound.Configuration config, SoundChannel channel) {
        super(config, channel);
    }
    
    /** @return the piano panel and all controls like On/Off switch, volume, velocity (for MIDI-channel only). */
    @Override
    public JComponent getKeyboard() {
        if (this.pianoPanel != null)
            return this.pianoPanel; // just one view, due to mouseHandler that stores UI-state
        
        final JRadioButton onOff = new JRadioButton(config.isVertical ? "" : "On/Off"); // no label when vertical
        onOff.setToolTipText("Turns Sound On and Off");
        onOff.setSelected(true);
        onOff.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (savedVolume == 0) {
                    savedVolume = volume;
                    savedVelocity = velocity;
                    volume = velocity = 0;
                    volumeSlider.setEnabled(false);
                    if (velocitySlider != null)
                        velocitySlider.setEnabled(false);
                }
                else {
                    volume = savedVolume;
                    velocity = savedVelocity;
                    savedVolume = 0;
                    volumeSlider.setEnabled(true);
                    if (velocitySlider != null)
                        velocitySlider.setEnabled(true);
                }
                getSoundChannel().volumeChange(volume);
            }
        });
        
        final boolean isWaveSound = getSoundChannel() instanceof WaveSoundChannel;
        
        getSoundChannel().volumeChange(volume);
        
        this.volumeSlider = new JSlider(0, 127, volume);
        volumeSlider.setToolTipText("Loudness");
        volumeSlider.setOrientation(config.isVertical ? SwingConstants.VERTICAL : SwingConstants.HORIZONTAL);
        volumeSlider.setBorder(BorderFactory.createTitledBorder((isWaveSound ? "Amplitude " : "Volume ")+volume));
        volumeSlider.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                volume = volumeSlider.getValue();
                if (isWaveSound) // has no velocity slider
                    velocity = volume;
                getSoundChannel().volumeChange(volume);
                volumeSlider.setBorder(BorderFactory.createTitledBorder((isWaveSound ? "Amplitude " : "Volume ")+volume));
            }
        });
        
        if (isWaveSound == false) {
            this.velocitySlider = new JSlider(0, 127, velocity);
            velocitySlider.setToolTipText("Piano <-> Forte");
            velocitySlider.setOrientation(config.isVertical ? SwingConstants.VERTICAL : SwingConstants.HORIZONTAL);
            velocitySlider.setBorder(BorderFactory.createTitledBorder("Touch Velocity "+velocity));
            velocitySlider.addChangeListener(new ChangeListener() {
                @Override
                public void stateChanged(ChangeEvent e) {
                    velocity = velocitySlider.getValue();
                    velocitySlider.setBorder(BorderFactory.createTitledBorder("Touch Velocity "+velocity));
                }
            });
        }
        
        final JPanel volumePanel = new JPanel();
        volumePanel.setLayout(new BoxLayout(volumePanel, config.isVertical ? BoxLayout.X_AXIS : BoxLayout.Y_AXIS));
        volumePanel.add(volumeSlider);
        if (velocitySlider != null)
            volumePanel.add(velocitySlider);
        
        this.controlPanel = new JPanel();
        controlPanel.setLayout(new BoxLayout(controlPanel, config.isVertical ? BoxLayout.Y_AXIS : BoxLayout.X_AXIS));
        controlPanel.add(onOff);
        controlPanel.add(volumePanel);
        
        final JComponent keyboardPanel = super.getKeyboard();
        
        final JPanel pianoPanel = new JPanel(new BorderLayout());
        pianoPanel.add(keyboardPanel, getPianoPanelBorderLayoutConstraint());
        pianoPanel.add(controlPanel, getControlPanelBorderLayoutConstraint());
        
        return this.pianoPanel = pianoPanel;
    }
    
    /** Locates the control-panel SOUTH (horizontal piano) or EAST (vertical piano). To be overridden. */
    protected String getPianoPanelBorderLayoutConstraint() {
        //return BorderLayout.CENTER;
        return config.isVertical ? BorderLayout.EAST : BorderLayout.SOUTH;
    }
    
    /** Locates the control-panel NORTH (horizontal piano) or WEST (vertical piano). To be overridden. */
    protected String getControlPanelBorderLayoutConstraint() {
        return config.isVertical ? BorderLayout.WEST : BorderLayout.NORTH;
    }
    
    /** @return the control panel (BoxLayout) where On/Off switch and volume slider is in. */
    protected Container getControlPanel() {
        return controlPanel;
    }

    /** @return the volume slider. */
    protected JSlider getVolumeSlider() {
        return volumeSlider;
    }

    /** @return the touch-velocity slider. */
    protected JSlider getVelocitySlider() {
        return velocitySlider;
    }

    /** Overridden to return user-adjustable touch-force. */
    @Override
    protected int getVelocity() {
        return velocity;
    }
}