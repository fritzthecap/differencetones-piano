package fri.music.instrument.wave;

interface HelpForIntervalLists
{
    String HTML = """
<html>
<head></head>
<body>

<h1>Lists of Intervals Generating Difference-Tones</h1>

<p>
Click onto a piano key below. A list of all intervals
that can generate the clicked key as difference-tone will be displayed, 
in the possibilities of the piano's tonal range (proposed 4 octaves).
The difference-tone will be displayed in the title-bar of the list frame.
</p><p>
If you click e.g. "C4", the highest interval
will be MINOR_THIRD "[F6 G#6]", the lowest MAJOR_SIXTH "[G4 E5]", 
for tuning <i>EqualTemperament</i>,
a deviation tolerance of 76 %,
and MINOR_THIRD / MAJOR_SIXTH as narrowest / widest intervals.
</p><p>
Mind that each tuning generates other difference-tones.
If you switch e.g. to <i>JustIntonation LIMIT_5_SYMMETRIC_1</i>,
the highest interval will be MINOR_THIRD "[E6 G6]". 
</p><p>
Tuning, deviation and interval-range affect the contents of these interval-lists.
You can close a list by clicking its "X" button in top right corner.
Below the list area there is a bar with actions that let control the lists.
</p>

<h2>List Actions</h2>
<p>
If you click into the title-bar of a list frame, 
the single tone for which the intervals have been calculated will be played on the piano.
</p><p>
If you click onto a list item representing an interval, 
that interval will be played on the piano, and its difference-tone will 
be marked by a red border and selection color.
It will sound although not being played - that is the nature of difference-tones.
You can hear it best with the "Sine" wave-form, as this has no overtones. 
</p>

<h2>Sort Lists by Pitch</h2>
<p>
If you deactivate this checkbox, the lists will be sorted in the 
order you click thekeys on piano. 
</p><p>
If you activate it, the lists will be ordered by the pitch
of their difference-tone, so they will be arranged parallel to the piano keys,
although much bigger.
This is useful if you have a melody's notes in mind and want to play it using the
interval lists instead of the piano keys.
</p>

<h2>Reuse Open Lists</h2>
<p>
If this checkbox is activated, a piano key will not open another interval list
when clicked a second time. This is useful to keep the number of list frames small.
</p><p>
Its deactivation makes sense when a sequence of tones 
is to be arranged in the order in which they appear in a melody, 
rather than sorted by pitch.
</p>

<h2>Detach Lists</h2>
<p>
If you click this button, the lists will be detached into a dialog window
that can be sized and moved around on the screen.
This creates place for a bigger number of list frames. 
The lists will be arranged like text in a flow from left to right, 
having line breaks according to the current size of the dialog window.
</p><p>
Closing the dialog window will move back all lists into the main window.
</p>

<h2>Close All Lists</h2>
<p>
As any piano key will open a list, it sometimes will be
useful to close them all together.
</p><p>
Mind that in combination with a notes text area, all lists may be reopened 
when their difference-tone is contained in the melody in the text-area 
and you make changes to that, by either clicking a piano key or editing the text.
</p>

</body>
</html>
""";
}