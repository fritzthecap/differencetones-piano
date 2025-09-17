package fri.music.instrument.notespiano.wave;

interface HelpForCompose
{
    String HTML = """
<html>
<head></head>
<body>

<h1>Composing Difference-Tone Intervals for a Melody</h1>
<p>
Target of difference-tone composition is to play a melody 
using difference-tone intervals instead of plain notes,
in other words, the composed intervals will generate the melody notes
as "shadow tones" (which is another name for difference-tones).
</p><p>
As a melody note can have several interval representations,
composing a difference-tone melody requires choosing one of these intervals. 
A melody with 1.3 octaves tone-range will need about 4 octaves above its lowest note, 
one with 2.3 octaves about 5 octaves, and so on.
</p><p>
This user-interface supports the process of difference-tone composition
by working from bottom left to top right.
Be aware that there is <b>no way to read files from hard-disk</b> except opening
it via a text editor, copying the text and ppasting it into the application.
Also there is <b>no way to save any data</b>
except copying it to some text editor and save it from there.
</p>

<h2>Recording a Melody</h2>
<p>
Bottom left you can click the melody notes on piano. 
Record the melody in the lower octave range, so that there is enough space for the intervals above.
On top of the piano, interval lists will be displayed for every clicked piano key.
If the checkbox "Write Notes" is on, every piano key will also be written 
into the "Melody" text-area above. Thus you can record a melody 
and then play it using the player on the left side of the text-area.
</p><p>
A note is a tone with a specific duration. 
The duration of the written notes can be controlled by either how long the mouse button is held down,
or by right mouse click and a duration context-menu, available on every piano key.
You can also edit the text, the duration is stored nowhere else.
Triplets and other special cases need to be edited manually in the text-area,
for syntax see the "Help" button below text-area.
For rests there is a button on top left of the piano.
Click it for writing the shown rest duration, or apply right-click to choose from
a duration context-menu.
</p><p>
The "Tempo" is for playing the melody by the recorder.
The "Time" signature is for formatting every bar into a separate text line, 
which makes it more readable.
</p><p>
Both are also for optional ABC export.
ABC is music notation language available on the Internet that can
generate visual note scores and also convert it to PDF and more.
Mind that you can not import ABC into this application!
</p>

<h2>Composing Intervals</h2>
<p>
As soon as you have written your melody in the left-side text-area
and the interval lists for all contained notes are visible (in whatever order you prefer),
you can begin to select intervals for the melody notes by clicking onto interval list items.
Make sure the right-side checkbox "Write Intervals" is on.
<b>You must select the intervals in the order of the melody notes</b>,
else they won't be written to the right-side text-area!
</p><p>
For that purpose it is useful to have the checkbox "Reuse Open Lists" deactivated (turned off).
That way you can work through the lists from left to right.
Else you need to have the melody notes order in your head and click the interval
list items in the according order. (But when you decide to reuse open lists,
then it will be useful to also turn on the checkbox "Sort Lists by Pitch", 
because then you have the lists in same order as piano keys are.)
</p><p>
As soon as the intervals are recorded, you can test the difference-tone melody
using the right-side player.
Any further click onto an interval list item will change the according interval text.
</p><p>
If you accidentally close all interval lists, you can open them again
by playing the melody on left side using its recorder, 
or by adding a space somewhere in the text-area.
</p><p>
Be warned about the loss of your work when you change 
parameters like tuning, deviation or narrowest/widest interval. 
Any such change will <b>erase the intervals text-area without warning</b>, 
because the intervals written there may not represent the melody any more. 
</p>

<h2>Auto-Compose</h2>
<p>
The "Auto-Compose" button will translate the left-side melody into
intervals and write that to the right-side text-area, so that you
can immediately play the difference-tone melody.
But do not expect the result to be beautiful. 
The algorithm that does the interval selections is quite naive and
will almost always need human corrections. 
Nevertheless you can use the "Auto-Compose" button as base for your sound selection.
</p>

<h2>Generate Melody</h2>
<p>
As soon as you have intervals in the right-side text-area and you
accidentally loose the left-side melody, you can re-generate the melody from the intervals.
This works reliably, because each interval can generate only one difference-tone.
But do not change tuning or any other parameter on top before, 
because any such change will erase the intervals text-area.
</p>

<h2>Write Intervals</h2>
<p>
This checkbox decides whether a click onto an interval list item will
be written into the intervals text-area or not.
Mind that this control is not the only reason why such clicks won't get recorded,
the most likely reason is that you didn't click the lists in order of
the melody notes.
</p>

</body>
</html>
""";
}