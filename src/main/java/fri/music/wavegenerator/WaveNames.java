package fri.music.wavegenerator;

/**
 * Utility to let choose wave forms.
 */
public class WaveNames
{
    /**
     * @return names of available wave-generators.
     *      To achieve the class name of a generator, 
     *      call <code>getClass(selectedWaveName)</code>.
     */
    public static String[] getNames() {
        return new String[] {
            SineWaveGenerator.class.getSimpleName().substring(0, "Sine".length()),
            TriangleWaveGenerator.class.getSimpleName().substring(0, "Triangle".length()),
            SquareWaveGenerator.class.getSimpleName().substring(0, "Square".length()),
            SawtoothWaveGenerator.class.getSimpleName().substring(0, "Sawtooth".length()),
        };
    }

    /**
     * @param waveName one of the names from <code>WaveNames.getNames()</code>.
     * @return the class that represents the generator for given wave form.
     */
    @SuppressWarnings("unchecked")
    public static Class<? extends WaveGenerator> getClass(String waveName) {
        final String packageName = WaveGenerator.class.getPackageName();
        final String simpleGeneratorClassName = WaveGenerator.class.getSimpleName();
        try {
            final Class<?> forName = Class.forName(packageName+"."+waveName+simpleGeneratorClassName);
            return (Class<? extends WaveGenerator>) forName;
        }
        catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }
}
