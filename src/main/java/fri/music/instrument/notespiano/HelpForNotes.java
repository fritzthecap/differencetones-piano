package fri.music.instrument.notespiano;

interface HelpForNotes
{
    String HTML = """
<html>
<head></head>
<body>

<h2>Write by Piano</h2>
<p>
When you turn on the "Write Notes" checkbox, 
you can use the piano to write notes.
Left click on any key writes to the text-area
at cursor position with a duration that is calculated
from the time the mouse button was down.
Right mouse click opens a context-menu that lets choose
the duration of the clicked piano key and then
writes to the text-area.
</p>

<h2>Notes Syntax</h2>
<p>
Every note is given as an IPN-name (international pitch notation)
and its duration behind a slash, for example: 
</p>
<ul>
<li>"A4/8" for a eighth note on A4 (4th octave) with pitch 440 Hz</li>
<li>"C#4/2." for a dotted C#4 half note (spans three quarter notes)</li>
<li>"-/4" for a quarter rest note</li>
<li>"E5/16~3" for a E5 triplet sixteenth note 
    (each of the triplets must have the "~3" tilde-postfix!);
    a quarter note triplet MUST start with a quarter note 
    (that may be tied to a subsequent one),
    but it MUST NOT start with an eighth note or a half note, 
    else it will be regarded as eighth or half triplet!</li>
<li>"[C4/4 E4 G4]" for a C-major chord, only the first note needs to have a length</li>
<li>"(G5/1 G5/1 G5/1)" for a G5 whole note that spans three 4/4 bars</li>
<li>"{A5/4 B5/4 C5/4}" for slurred notes</li>
</ul>
<p>
No space must appear between a note and its duration specification,
but at least one whitespace MUST be between different notes.
Spaces are allowed between notes and tie-, slur- and chord-symbols.
</p><p>
In IPN there is no "Eb" or "Bb", you must give "D#" or "A#",
and there is no German "H", such is written as "B".
But you can use both lower or upper case letters in IPN-names.
</p><p>
The <b>tempo</b> can appear as simple BPM-number (beats per minute)
on top of the notes only, it can not change in-between.
It always refers to a quarter note.
</p><p>
The <b>time signature</b> can appear on top of the notes, or everywhere in-between,
written as "4/4" or "3/4" or similar.
</p><p>
Do not care about bars, the player will automatically calculate bar bounds
using the given time signature(s).
You can use the "Format" button to put every bar into a separate line.
</p>

<h3>Ties</h3>
<p>
Notes connected by a "tie" are notes of same pitch that are played as single note, 
even across several bars.
Ties are started by an opened parenthesis "(" and ended by a closed ")",
notes in between MUST NOT be enclosed in parentheses (no nested parentheses).
</p>

<h3>Slurs</h3>
<p>
Notes connected by a "slur" are notes of different pitch that are phrased together, 
even across several bars.
Slurs are started by an opened brace "{" and ended by a closed "}",
notes in between MUST NOT be enclosed in "{...}" (no nested braces),
because it is not clear how to phrase several notes that are all slurred together.
</p>

<h3>Chords</h3>
<p>
Chord notes are two or more notes of different pitch that are played simultaneously.
Chords are started by an opened bracket "[" and ended by closed "]", 
just the first note needs to have a duration, it is assumed that all have the same length.
Ties and slurs from inside to outside of a chord are not allowed, but you can tie chords,
even across bars.
</p>

<h2>Editor Actions</h2>
<p>
With right mouse click you can open a context-menu that provides
some useful text-area actions:
</p>
<ul>
<li><b>Ctrl-x</b> for "Cut" selection</li>
<li><b>Ctrl-c</b> for "Copy" selection</li>
<li><b>Ctrl-v</b> for "Paste" at caret position</li>
<li><b>Ctrl-z</b> for "Undo" last action</li>
<li><b>Ctrl-y</b> for "Redo" last "Undo"</li>
<li><b>Ctrl-a</b> for "Select All" text</li>
</ul>

</body>
</html>
""";
}