package fri.music.player.notelanguage;

import fri.music.player.Note;

/** Helper that tracks the millisecond position in bar. */
class BarState
{
    private final int barDurationMilliseconds;
    private final int barDurationMillisecondsHalf;
    private final boolean isBarHalvable;
    
    private int currentMillis = 0;
    
    BarState(int numberOfBeatsPerBar, int beatDurationMilliseconds) {
        this.barDurationMilliseconds = (numberOfBeatsPerBar * beatDurationMilliseconds);
        this.barDurationMillisecondsHalf = (int) Math.round((double) barDurationMilliseconds / 2.0);
        this.isBarHalvable = 
                numberOfBeatsPerBar % 2 == 0 && // must be divisible by 2
                numberOfBeatsPerBar % 3 != 0 &&
                numberOfBeatsPerBar % 5 != 0 &&
                numberOfBeatsPerBar % 7 != 0 &&
                numberOfBeatsPerBar % 11 != 0;
                // do not accept  6/8, 10/8, 12/8, 18/8, 20/8, ... 
                // as you never know how they are sub-divided, thus there could be a wrong accentuation
    }
    
    /** MUST call this to add a note to the current bar. */
    public void add(int durationMilliseconds) {
        currentMillis += durationMilliseconds;
        
        if (matches(currentMillis, barDurationMilliseconds))
            currentMillis = 0;
        else if (currentMillis > barDurationMilliseconds)
            currentMillis = barDurationMilliseconds - currentMillis; // negative when exceeding bar end
    }
    
    /** @return true when currently being on the first beat of the bar. */
    public boolean isBarStart() {
        return currentMillis == 0;
    }
    
    /** @return true when currently being on half of the bar, always false if bar-meter is not halvable. */
    public boolean isBarHalf() {
        return isBarHalvable && // can be reliably divided into 2 parts
                matches(currentMillis, barDurationMillisecondsHalf);
    }

    /** @return the (positive) excess amount when currently the bar was exceeded, else zero. */
    public int isBarExceed() {
        return (currentMillis < 0) ? (-currentMillis) : 0;
    }

    private boolean matches(int currentMillis, int limit) {
        return currentMillis + Note.MINIMAL_DURATION >= limit && 
                currentMillis - Note.MINIMAL_DURATION <= limit;
    }
}