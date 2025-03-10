package fri.music.differencetones.composer;

import fri.music.Tone;

/**
 * A tone could have different interval-representations (as difference-tone),
 * thus the index of the tone within the melody is needed.
 */
public record NoteWithIndex(Tone tone, int melodyIndex)
{
}
