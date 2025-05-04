package fri.music.justintonation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import fri.music.AbstractJustIntonation.Interval;
import fri.music.JustIntonation;
import fri.music.AbstractJustIntonation.ChromaticScale;
import fri.music.MathUtils;
import fri.music.ScaleTypes;
import fri.music.ToneSystem;

/**
 * Checks if all occurrences of a certain interval (third, fourth, fifth, sixth)
 * in a diatonic scale have the same ratio, determined by a given just-intonation tuning.
 * <p/>
 * For simplicity, it always builds a chromatic scale based on C, because any just-intonation
 * tuning must be calculated by its ratios from a given base-tone, thus all chromatic scales
 * would will be the same, no matter if they are built on C, C#, D, etc.
 * <p/>
 * A given set of diatonic modal scales will be built from the notes of the chromatic scale.
 * These modal scales will start at their natural base-note and use white piano-keys only, like
 * <i>(PHRYGIAN = E F G A ..., AEOLIAN = A B C ...)</i>.
 * <p/>
 * Then it checks whether all minor-thirds are the same, i.e. have the same ratio-distance.
 * By default, it checks this for all intervals except minor-second and major-seventh, 
 * as these are very dissonant anyway. In other words, it checks the consistency of
 * the scale's harmonious intervals. Optionally the check will be performed using 5-limit
 * intervals instead of the scale's own intervals.
 * <p/>
 * A check can be done also for triads, as these are only a stack of intervals.
 * The intervals in a triad can be minor/major-thirds, fourth, fifth, minor/major-sixths, 
 * thus not triads but chords with five notes are actually checked for consistency,
 * e.g. C4-E4-G4-C5-E5.
 * <p/>
 * The application counts and reports all inconsistencies, optionally also consistencies.
 * Optionally it considers an interval to be dirty when the ratio of its difference-tone
 * can not be found in the underlying diatonic scale.
 */
public class JustIntonationChecker
{
    public static final String newline = System.getProperty("line.separator");
    
    /** Options to pass to JustIntonationChecker constructor. */
    public static class Configuration
    {
        public final boolean showScalesOnly;
        public final boolean showUnjustOnly;
        public final boolean checkAgainst5LimitIntervals;
        public final boolean alsoCheckMajorSecondAndMinorSeventh;
        public final List<String> checkedScaleNames;
        public final boolean checkWhiteKeyScalesOnly;
        public final boolean showTriadsOnly;
        public final boolean considerDifferenceTones;
        public final boolean checkChromaticScaleDifferenceTones;

        /** Default constructor. */
        public Configuration() {
            this(null, null, null, null, null, null, null, null, null);
        }
        
        /**
         * @param showScalesOnly default is false, do not display details of intervals and triads, show just scale diagnosis.
         * @param showUnjustOnly default is true, when displaying interval or triad details,
         *      then show the unjust intervals and triads only, leaving out just intervals and triads.
         * @param checkAgainst5LimitIntervals default is false, compare the diatonic scale-intervals with
         *      intervals extracted from its chromatic scale template, not with the 5-limit standard intervals.
         *      When false, you check if the scale is coherent in itself, no matter if you like or dislike pythagoreian thirds.
         *      When true, you declare 5-limit intervals with its simple ratios as "the beauty standard".
         * @param alsoCheckMajorSecondAndMinorSeventh default is false, check not only both thirds, fourth, fifth and both sixth, 
         *      but also major second and minor seventh.
         * @param checkedScaleNames list of modal scale names (IONIAN, AEOLIAN, ...), built on given chromatic scale,
         *      that should be checked. When null or empty, all scales will be checked. 
         * @param checkWhiteKeyScalesOnly default is true, inspect only the scale variant starting on scale's root note 
         *      (on a piano keyboard that scale would consist only of white keys), not also the variant starting on C.
         * @param showTriadsOnly default is false, display triad diagnosis only, no intervals.
         * @param considerDifferenceTones default is true, check if difference-tones of intervals are in scale and set
         *      interval unjust when not.
         * @param checkChromaticScaleDifferenceTones default is false, check all intervals of
         *      white keys of the given chromatic scale for their difference-tones being contained in scale.
         */
        public Configuration(
                Boolean showScalesOnly, 
                Boolean showUnjustOnly,
                Boolean checkAgainst5LimitIntervals,
                Boolean alsoCheckMajorSecondAndMinorSeventh,
                List<String> checkedScaleNames,
                Boolean checkWhiteKeyScalesOnly,
                Boolean showTriadsOnly,
                Boolean considerDifferenceTones,
                Boolean checkChromaticScaleDifferenceTones)
        {
            this.showScalesOnly = (showScalesOnly != null) ? showScalesOnly : false;
            this.showUnjustOnly = (showUnjustOnly != null) ? showUnjustOnly : true;
            this.checkAgainst5LimitIntervals = (checkAgainst5LimitIntervals != null) ? checkAgainst5LimitIntervals : false;
            this.alsoCheckMajorSecondAndMinorSeventh = (alsoCheckMajorSecondAndMinorSeventh != null) ? alsoCheckMajorSecondAndMinorSeventh : false;
            this.checkedScaleNames = (checkedScaleNames != null && checkedScaleNames.size() > 0) ? checkedScaleNames : null;
            this.checkWhiteKeyScalesOnly = (checkWhiteKeyScalesOnly != null) ? checkWhiteKeyScalesOnly : true;
            this.showTriadsOnly = (showTriadsOnly != null) ? showTriadsOnly : false;
            this.considerDifferenceTones = (considerDifferenceTones != null) ? considerDifferenceTones : true;
            this.checkChromaticScaleDifferenceTones = (checkChromaticScaleDifferenceTones != null) ? checkChromaticScaleDifferenceTones : false;
        }
    }
    
    
    /** Represents the check-results for all harmony-relevant intervals of one chromatic scale. */
    public record Result(
            ChromaticScale chromaticScale, 
            List<DiatonicScaleCheckResult> diatonicScaleCheckResults,
            List<ChromaticScaleCheckResult> chromaticScaleCheckResults,
            Configuration configuration)
    {
        @Override
        public final String toString() {
            final StringBuilder sb = new StringBuilder();
            
            sb.append("Chromatic Scale "+chromaticScale().name()+": ");
            final String intervals = Stream.of(chromaticScale().intervals())
                    .map(interval -> interval.ratioString(0))
                    .collect(Collectors.joining(", "));
            sb.append(intervals+newline);
            
            final List<ChromaticScaleCheckResult> chromaticScaleResults = chromaticScaleCheckResults();
            if (chromaticScaleResults.size() > 0) {
                final long unjustDifferenceTones = chromaticScaleResults.stream()
                    .filter(result -> result.differenceTone == null)
                    .count();
                sb.append(
                        "Found "+unjustDifferenceTones+" unjust difference-tones in "+
                        chromaticScaleResults.size()+" checked intervals"+
                        (unjustDifferenceTones > 0 ? ":" : "!")+newline);
                
                for (ChromaticScaleCheckResult scaleResult : chromaticScaleResults)
                    sb.append(scaleResult.toString(configuration));
            }
            
            final List<DiatonicScaleCheckResult> diatonicScaleResults = diatonicScaleCheckResults();
            final DiatonicScaleCheckResult firstResult = diatonicScaleResults.get(0); // all were checked against the same intervals
            
            final int leastCommonMultiple = MathUtils.leastCommonMultipleInt(
                    firstResult.diatonicScaleIntervals().stream().map(interval -> interval.divisor()));
            final String diatonicIntervalRatios = firstResult.diatonicScaleIntervals().stream()
                    .skip(1) // skip leading UNISON 1/1
                    .limit(6) // display just first octave, leaving out trailing OCTAVE 2/1
                    .map(interval -> interval.ratioString())
                    .collect(Collectors.joining(", "));
            sb.append("Diatonic LCM = "+leastCommonMultiple+" (least common multiple of diatonic divisors in "+diatonicIntervalRatios+")"+newline);
            
            final String harmonicIntervals = firstResult.harmonicIntervals().stream()
                    .map(interval -> interval.name()+"("+interval.ratioString(0)+")")
                    .collect(Collectors.joining(", "));
            sb.append("Checking "+firstResult.harmonicIntervals().size()+" intervals: "+harmonicIntervals+newline);
            
            sb.append(newline);
            
            final Set<IntervalCheckResult> unjustIntervalsUnique = diatonicScaleResults.stream()
                    .map(r -> r.unJustIntervals())
                    .reduce(new HashSet<IntervalCheckResult>(), (o1, o2) -> { o1.addAll(o2); return o1; });
            final Set<String> unjustTriadsUnique = diatonicScaleResults.stream()
                    .map(r -> r.unJustTriads())
                    .reduce(new HashSet<String>(), (o1, o2) -> { o1.addAll(o2); return o1; });
            sb.append("Found "+
                    unjustIntervalsUnique.size()+" unjust intervals and "+ // contains no repetitions
                    unjustTriadsUnique.size()+" unjust triads in "+diatonicScaleCheckResults.size()+" diatonic scales:"+ // contains no repetitions
                    newline);

            for (DiatonicScaleCheckResult scaleResult : diatonicScaleResults)
                sb.append(scaleResult.toString(configuration));
                
            return sb.toString();
        }
    }
    
    
    /** Represents the check-results for difference-tones of all intervals of the chromatic scale. */
    public record ChromaticScaleCheckResult(
            String lowerNoteName,
            String upperNoteName,
            int semitoneDistance,
            IntervalWithOctave differenceTone)
    {
        public final String toString(Configuration configuration) {
            if (configuration.showUnjustOnly && differenceTone != null)
                return "";
            
            final String intervalName = ToneSystem.intervalName(semitoneDistance());
            return "\t"+intervalName+"\t"+lowerNoteName()+"->"+upperNoteName()+
                    (differenceTone != null 
                        ? "\tDifference-tone in scale:\t"+differenceTone.targetNoteName+"["+differenceTone.ipnOctave+"]"
                        : "\tDifference-tone NOT in scale!")+
                    newline;
        }
    }
    
    
    /** Represents the check-results for all harmony-relevant intervals of a diatonic modal scale. */
    public record DiatonicScaleCheckResult(
            String scaleInfo, 
            List<Interval> harmonicIntervals,
            List<IntervalWithOctave> diatonicScaleIntervals,
            Map<String,List<IntervalCheckResult>> intervalResults,
            Set<IntervalCheckResult> unJustIntervals,
            List<TriadCheckResult> triadResults,
            Set<String> unJustTriads)
    {
        public String toString(Configuration configuration) {
            final StringBuilder sb = new StringBuilder();
            
            sb.append("\tScale "+scaleInfo()+":"+newline);
            
            final long unustTriads = triadResults().stream().filter(r -> r.isJust() == false).count();
            final String triadTitle = "\t\tTriads: "+unustTriads+" unjust of "+triadResults().size()+newline;

            final long allCheckedIntervals = intervalResults().entrySet().stream().flatMap(e -> e.getValue().stream()).count();
            final String intervalTitle = "\t\tIntervals: "+unJustIntervals().size()+" unjust of "+allCheckedIntervals+newline;
            
            if (configuration.showScalesOnly == true) { // no triad or interval details
                sb.append(triadTitle);
                
                if (configuration.showTriadsOnly == false)
                    sb.append(intervalTitle);
            }
            else { // configuration.showScalesOnly == false
                boolean titleAppended = false;
                for (TriadCheckResult triadResult : triadResults()) {
                    final String output = triadResult.toString(configuration, triadTitle, titleAppended);
                    if (output.length() > 0) {
                        titleAppended = true;
                        sb.append(output);
                    }
                }
                
                if (configuration.showTriadsOnly == false) {
                    if (configuration.showUnjustOnly == false || unJustIntervals().size() > 0)
                        sb.append(intervalTitle);
                    
                    for (Map.Entry<String,List<IntervalCheckResult>> intervalResult : intervalResults().entrySet()) {
                        final String title = "\t\t\t"+intervalResult.getKey()+newline; // interval name
                        titleAppended = false;
                        for (IntervalCheckResult checkResult : intervalResult.getValue()) {
                            final String output = checkResult.toString(configuration, title, titleAppended);
                            if (output.length() > 0) {
                                titleAppended = true;
                                sb.append(output);
                            }
                        }
                    }
                }
            }
            
            return sb.toString();
        }
    }
    
    
    /** Represents the check-result for one harmony-relevant interval of a modal scale. */
    public record IntervalCheckResult(
            Interval checkedInterval, // TODO: remove this unused member
            String lowerNote, 
            String upperNote, 
            String expectedRatio, 
            String actualRatio,
            boolean isJust,
            int centError,
            String differenceToneComment)
    {
        public String toString(Configuration configuration, String title, boolean titleAppended) {
            final StringBuilder sb = new StringBuilder();
            
            if (configuration.showUnjustOnly == false || isJust() == false) {
                if (titleAppended == false)
                    sb.append(title);
                
                sb.append("\t\t\t\t"+lowerNote()+"->"+upperNote()+":\t");
                sb.append(
                        (isJust() ? "OK" : differenceToneComment() != null ? differenceToneComment() : "unjust")+
                        (differenceToneComment() == null // when not null, ratios do not matter
                            ? ",\texpected: "+expectedRatio()+", actual: "+actualRatio()+
                                (isJust() || centError() == 0 ? "" : ", "+(centError() > 0 ? "+"+centError() : centError())+" cent")
                            : "")+
                        newline);
            }
            
            return sb.toString();
        }
    }
    
    
    public record TriadCheckResult(
            String triadName,
            String notes,
            boolean isJust,
            String unjustReasons)
    {
        public String toString(Configuration configuration, String title, boolean titleAppended) {
            final StringBuilder sb = new StringBuilder();
            
            if (configuration.showUnjustOnly == false || isJust() == false) {
                if (titleAppended == false)
                    sb.append(title);
                
                sb.append("\t\t\t"+triadName()+notes()+":\t");
                sb.append(
                        (isJust() ? "OK" : "unjust:\t"+unjustReasons())+
                        newline);
            }
            
            return sb.toString();
        }
    }



    public static final JustIntonation.ChromaticScales LIMIT5_COMPARISON_SCALE = JustIntonation.ChromaticScales.LIMIT_5_SYMMETRIC_1;
    
    private final Configuration configuration;
    
    public JustIntonationChecker(Configuration configuration) {
        this.configuration = configuration;
    }
    
    /**
     * @param chromaticScale the scale to check for unjust intervals.
     * @return a diagnosis result containing details about all diatonic 7-tone scales (modes)
     *      built using given chromatic 12-tone scale.
     */
    public Result check(ChromaticScale chromaticScale) {
        final List<ChromaticScaleCheckResult> chromaticCheckResults = new ArrayList<>();
        if (configuration.checkChromaticScaleDifferenceTones) {
            checkChromaticScaleDifferenceTones(chromaticScale, chromaticCheckResults);
        }
        
        // Build all modal scale variants
        final List<DiatonicScaleCheck> diatonicScales = buildDiatonicScaleCheckers(chromaticScale);
        
        // Check intervals in all diatonic modal scales for harmonic consistency
        final List<DiatonicScaleCheckResult> diatonicCheckResults = new ArrayList<>();
        for (DiatonicScaleCheck diatonicScale : diatonicScales) {
            diatonicCheckResults.add(diatonicScale.checkAgainstHarmonicIntervals(diatonicScale.whiteKeyIntervals));
            
            if (configuration.checkWhiteKeyScalesOnly == false)
                diatonicCheckResults.add(diatonicScale.checkAgainstHarmonicIntervals(diatonicScale.cBasedIntervals));
        }
        
        return new Result(chromaticScale, diatonicCheckResults, chromaticCheckResults, configuration);
    }

    /** Checks all semi-tones (except MINOR_SECOND) of the chromatic scale for difference-tones being in scale. */
    private void checkChromaticScaleDifferenceTones(
            ChromaticScale chromaticScale, 
            List<ChromaticScaleCheckResult> chromaticCheckResults)
    {
        // C-D, C-D#, C-E, C-F, C-F#, C-G, C-G#, C-A, C-A#, C-B
        // D-E, D-F, D-F#, ..., D-C#
        // ....
        final List<String> chromaticNoteNames = Arrays.asList(ToneSystem.IPN_BASE_NAMES);
        final int noteNamesSize = chromaticNoteNames.size();
        final ChromaticScaleOctaves chromaticScaleOctaves = new ChromaticScaleOctaves(chromaticScale, 2);
        
        for (int lowerIndex = 0; lowerIndex < noteNamesSize; lowerIndex++) {
            final String lowerNote = chromaticNoteNames.get(lowerIndex);
            
            final boolean CHECK_ALL_SEMITONES = false;
            if (CHECK_ALL_SEMITONES || (lowerNote.endsWith("#") == false)) { // check only white keys
                final int lowerOctave = lowerIndex / noteNamesSize;
                
                int semitoneDistance = 2;
                for (int upperIndex = lowerIndex + semitoneDistance; 
                        upperIndex < lowerIndex + noteNamesSize + 1; 
                        upperIndex++, semitoneDistance++)
                {
                    if (CHECK_ALL_SEMITONES || semitoneDistance < 10) { // no more MINOR_SEVENTH
                        // stop at major sixth (A), seventh (A#, B) has very diffuse difference-tones
                        final String upperNote = chromaticNoteNames.get(upperIndex % noteNamesSize);
                        final int upperOctave = upperIndex / noteNamesSize;
                        
                        final IntervalWithOctave existsInScale = chromaticScaleOctaves.existsDifferenceToneInScale(lowerNote, upperNote);
                        ChromaticScaleCheckResult result = new ChromaticScaleCheckResult(
                                lowerNote+"["+lowerOctave+"]",
                                upperNote+"["+upperOctave+"]",
                                semitoneDistance,
                                existsInScale);
                        chromaticCheckResults.add(result);
                    }
                }
            }
        }
        chromaticCheckResults.sort((r1, r2) -> r1.semitoneDistance() - r2.semitoneDistance()); // second first, sixth last
    }

    private List<DiatonicScaleCheck> buildDiatonicScaleCheckers(ChromaticScale chromaticScale) {
        final List<DiatonicScaleCheck> diatonicScales = new ArrayList<>();
        
        for (Map.Entry<String,boolean[]> scaleEntry : ScaleTypes.scaleToLayout.entrySet()) {
            final String scaleName = scaleEntry.getKey();
            
            if (configuration.checkedScaleNames == null || configuration.checkedScaleNames.contains(scaleName)) {
                final String lowestNote = ScaleTypes.scaleToStartNote.get(scaleName);
                
                diatonicScales.add(new DiatonicScaleCheck(
                        scaleName, 
                        scaleEntry.getValue(), // layout
                        chromaticScale, 
                        ScaleTypes.cBasedSemitoneIndex(lowestNote),
                        configuration));
            }
        }
        return diatonicScales;
    }
    
    
    private static class DiatonicScaleCheck extends DiatonicScaleOctaves
    {
        private final Configuration configuration;
        
        DiatonicScaleCheck(
                String scaleName, 
                boolean[] scaleLayout, // white key = true, black key = false
                ChromaticScale chromaticScale, 
                int scaleBaseSemitoneOffset,
                Configuration configuration)
        {
            super(scaleName, 
                    scaleLayout, // white key = true, black key = false
                    chromaticScale, 
                    scaleBaseSemitoneOffset,
                    configuration.checkWhiteKeyScalesOnly == false);
            this.configuration = configuration;
        }

        /**
         * Checks given scale variant against all consonant intervals and returns the check-result.
         * The implemented intervals are minor and major third, fourth, fifth, and minor and major sixth. 
         * The diatonic scale will be looped up to check if e.g. all found minor thirds are of the same ratio as
         * the first minor third found in chromatic scale.
         * @param diatonicScaleIntervals the list of diatonic intervals of a scale to check.
         * @return the result of the check.
         */
        public DiatonicScaleCheckResult checkAgainstHarmonicIntervals(List<IntervalWithOctave> diatonicScaleIntervals) {
            final Map<String,List<IntervalCheckResult>> intervalCheckResults = new LinkedHashMap<>();
            final Set<IntervalCheckResult> unjustIntervalsUnique = new HashSet<>();
            
            final List<Interval> harmoniousIntervals = harmoniousIntervals();
            for (Interval harmonicInterval : harmoniousIntervals) {
                final List<IntervalCheckResult> oneIntervalResults = checkIntervalInScale(diatonicScaleIntervals, harmonicInterval);
                
                final Set<IntervalCheckResult> unJust = new HashSet<>(oneIntervalResults.stream()
                        .filter(result -> result.isJust() == false)
                        .toList());
                unjustIntervalsUnique.addAll(unJust);
                
                final String title = harmonicInterval.name()+": "+(unJust.size() <= 0 ? "" : unJust.size()+" unjust of "+oneIntervalResults.size());
                intervalCheckResults.put(title, oneIntervalResults);
            }
            
            final List<TriadCheckResult> triadCheckResults = new ArrayList<>();
            final List<IntervalCheckResult> allIntervalCheckResults = intervalCheckResults.values().stream()
                    .flatMap(list -> list.stream())
                    .toList();
            for (Triad triad : Triad.values()) {
                final TriadCheckResult result = checkTriad(diatonicScaleIntervals, allIntervalCheckResults, triad);
                triadCheckResults.add(result);
            }
            final Set<String> unJustTriads = new HashSet<>(triadCheckResults.stream()
                    .filter(result -> result.isJust() == false)
                    .map(result -> result.notes())
                    .toList());
            
            final String noteNames = diatonicScaleIntervals.stream()
                    .map(interval -> interval.targetNoteName)
                    .limit(7)
                    .collect(Collectors.joining(","));
            
            return new DiatonicScaleCheckResult(
                    scaleName+"("+noteNames+")",
                    harmoniousIntervals,
                    diatonicScaleIntervals,
                    intervalCheckResults,
                    unjustIntervalsUnique,
                    triadCheckResults,
                    unJustTriads);
        }
        
        
        /**
         * @param lowerTone the lower tone of the found interval.
         * @param upperTone the higher tone of the found interval.
         */
        public record ScaleLoopItem(IntervalWithOctave lowerTone, IntervalWithOctave upperTone)
        {
        }
        
        /** Lists all instances of a given interval in a scale. */
        private class ScaleLoop implements Iterable<ScaleLoopItem>
        {
            private final List<ScaleLoopItem> items = new ArrayList<>();
            private int currentIndex;
            
            ScaleLoop(List<IntervalWithOctave> diatonicScaleIntervals, Interval intervalToCheck) {
                final int maxOccurrences = maxOccurrences(intervalToCheck);
                final int semitoneStepsToMatch = semitones(intervalToCheck);
                int occurrences = 0;
                for (int i = 0; occurrences < maxOccurrences && i < diatonicScaleIntervals.size(); i++) {
                    final IntervalWithOctave upperTone = diatonicScaleIntervals.get(i);
                    final IntervalWithOctave lowerTone = searchBack(diatonicScaleIntervals, i, semitoneStepsToMatch);
                    if (lowerTone != null) {
                        items.add(new ScaleLoopItem(lowerTone, upperTone));
                        occurrences++;
                    }
                }
            }
            
            @Override
            public Iterator<ScaleLoopItem> iterator() {
                return new Iterator<>() {
                    @Override
                    public boolean hasNext() {
                        return currentIndex < items.size();
                    }
                    @Override
                    public ScaleLoopItem next() {
                        final ScaleLoopItem item = items.get(currentIndex);
                        currentIndex++;
                        return item;
                    }
                };
            }
            
            private IntervalWithOctave searchBack(List<IntervalWithOctave> intervals, int startIndex, int semitoneStepsToMatch) {
                int semitoneSteps = intervals.get(startIndex).semitoneStepsFromPredecessor;
                for (int i = startIndex - 1; i >= 0 && semitoneSteps <= semitoneStepsToMatch; i--) {
                    final IntervalWithOctave interval = intervals.get(i);
                    if (semitoneSteps == semitoneStepsToMatch)
                        return interval;
                    semitoneSteps += interval.semitoneStepsFromPredecessor;
                }
                return null;
            }
        }   // end class ScaleLoop
        
        
        /**
         * In given diatonicScaleIntervals, find all that match intervalToCheck and return their diagnosis.
         * @param diatonicScaleIntervals the list to use for checks of all occurrences of given interval.
         * @param intervalToCheck the interval to check.
         * @return the result of the check.
         */
        private List<IntervalCheckResult> checkIntervalInScale(List<IntervalWithOctave> diatonicScaleIntervals, Interval intervalToCheck) {
            final List<IntervalCheckResult> results = new ArrayList<>();
            for (ScaleLoopItem item : new ScaleLoop(diatonicScaleIntervals, intervalToCheck)) {
                final long[] distance = IntervalWithOctave.distance(item.lowerTone, item.upperTone);
                final int actualDividend = (int) distance[0];
                final int actualDivisor = (int) distance[1];
                
                final String actualRatio = actualDividend+"/"+actualDivisor;
                final String expectedRatio = intervalToCheck.dividend(0)+"/"+intervalToCheck.divisor();
                final boolean isJust = expectedRatio.equals(actualRatio);
                final int centDiff = isJust ? 0
                    : (int) Math.round(Interval.cent(actualDividend, actualDivisor) - intervalToCheck.cent(0));
                
                // check if the interval's difference-tone is in scale
                String differenceToneComment = null;
                if (isJust && configuration.considerDifferenceTones) {
                    final long[] differenceRatio = IntervalWithOctave.difference(item.lowerTone, item.upperTone);
                    final IntervalWithOctave differenceTone = IntervalWithOctave.existsDifferenceToneInScale(diatonicScaleIntervals, differenceRatio);
                    if (differenceTone == null) {
                        differenceToneComment = "Difference-tone not in scale";
                        /*
                        System.err.println(intervalToCheck.name()+(isJust ? "" : "(unjust)")+": "+
                                item.lowerTone.targetNoteName()+item.lowerTone.ipnOctave()+
                                " and "+item.upperTone.targetNoteName()+item.upperTone.ipnOctave()+
                                (differenceTone != null 
                                    ? " produce "+differenceTone.targetNoteName()+differenceTone.ipnOctave() 
                                    : " difference-tone "+differenceRatio[0]+"/"+differenceRatio[1]+" not in scale"));
                         */
                    }
                }
                
                results.add(new IntervalCheckResult(
                        intervalToCheck,
                        item.lowerTone.targetNoteName,
                        item.upperTone.targetNoteName,
                        expectedRatio,
                        actualRatio,
                        isJust && differenceToneComment == null,
                        centDiff,
                        differenceToneComment));
            }
            return results;
        }
        
        private List<Interval> harmoniousIntervals() {
            final List<Interval> intervals = Stream.of(
                    configuration.checkAgainst5LimitIntervals
                        ? LIMIT5_COMPARISON_SCALE.intervals()
                        : chromaticIntervals
                ).toList();
            
            final List<Interval> harmoniousIntervals = new ArrayList<>();
            
            if (configuration.alsoCheckMajorSecondAndMinorSeventh) // useless to check dissonant seconds and seventh
                harmoniousIntervals.add(findInterval(intervals, interval -> isMajorSecond(interval)));
            
            harmoniousIntervals.add(findInterval(intervals, interval -> isMinorThird(interval)));
            harmoniousIntervals.add(findInterval(intervals, interval -> isMajorThird(interval)));
            harmoniousIntervals.add(findInterval(intervals, interval -> isFourth(interval)));
            harmoniousIntervals.add(findInterval(intervals, interval -> isFifth(interval)));
            harmoniousIntervals.add(findInterval(intervals, interval -> isMinorSixth(interval)));
            harmoniousIntervals.add(findInterval(intervals, interval -> isMajorSixth(interval)));
            
            if (configuration.alsoCheckMajorSecondAndMinorSeventh)
                harmoniousIntervals.add(findInterval(intervals, interval -> isMinorSeventh(interval)));
            
            return harmoniousIntervals;
        }
        
        private Interval findInterval(List<Interval> intervals, Predicate<Interval> predicate) {
            return intervals.stream()
                    .filter(predicate)
                    .findFirst()
                    .orElseThrow();
        }
        
        private TriadCheckResult checkTriad(
                List<IntervalWithOctave> diatonicScaleIntervals,
                List<IntervalCheckResult> allIntervalCheckResults, 
                Triad triad) 
        {
            if (triad.diatonicTones1ToN.length != 5)
                throw new IllegalArgumentException("Triad must have 5 elements, but has "+triad.diatonicTones1ToN.length);
                
            // turn the chord indexes to note names of the given scale
            final List<String> triadNoteNames = new ArrayList<>();
            for (int triadIndex1ToN : triad.diatonicTones1ToN)
                triadNoteNames.add(diatonicScaleIntervals.get(triadIndex1ToN - 1).targetNoteName);

            // check all intervals contained in chord, without repetitions, keeping order
            String triadName = triad.name();
            String unjustReasons = "";
            for (int[] toneTupleIndexes : Triad.intervalIndexTuples) {
                final String lowerNote = triadNoteNames.get(toneTupleIndexes[0]);
                final String upperNote = triadNoteNames.get(toneTupleIndexes[1]);
                
                boolean found = false;
                for (int i = 0; found == false && i < allIntervalCheckResults.size(); i++) {
                    final IntervalCheckResult intervalCheckResult = allIntervalCheckResults.get(i);
                    
                    if (intervalCheckResult.lowerNote().equals(lowerNote) &&
                            intervalCheckResult.upperNote().equals(upperNote))
                    {
                        found = true;
                        
                        if (intervalCheckResult.isJust() == false)
                            unjustReasons += (unjustReasons.length() > 0 ? ", " : "")+lowerNote+"->"+upperNote;
                        
                        if (triad.equals(Triad.LEADING) && // for minor scales it is SUBTONIC instead of LEADING
                                scaleName.equals(ScaleTypes.IONIAN) == false && scaleName.equals(ScaleTypes.LYDIAN) == false)
                            triadName = "SUBTONIC";
                    }
                }
            }
            
            final String noteNames = "("+triadNoteNames.stream().collect(Collectors.joining(","))+")";
            return new TriadCheckResult(triadName, noteNames, unjustReasons.length() <= 0, unjustReasons);
        }
    }   // end class DiatonicScaleCheck
    
}   // end class JustIntonationChecker
