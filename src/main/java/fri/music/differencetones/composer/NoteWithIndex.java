package fri.music.differencetones.composer;

import fri.music.Tone;

/**
 * A tone, as difference-tone, could have different interval-representations,
 * thus the index of the tone within the melody is needed.
 */
public record NoteWithIndex(Tone tone, int melodyIndex)
{
}
