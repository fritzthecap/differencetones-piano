package fri.music.demos;

import fri.music.wavegenerator.SineWaveGenerator;

public class SineWaveGeneratorDemo
{
//  public static void main(String[] args) throws Exception {
//      SineWaveGenerator sineWaveGenerator = new SineWaveGenerator();
//      
//      int volume = MAXIMUM_VOLUME / 3;
//      // getting warm
//      sineWaveGenerator.play(220, 100, volume); // play many seconds
//      
//      // measuring
//      System.err.println("Playing 440 Hertz");
//      System.err.println(new java.util.Date());
//      
//      sineWaveGenerator.play(440, 10 * 1000, volume); // play many seconds
//      
//      System.err.println(new java.util.Date());
//      
//      // measuring
//      System.err.println("Playing 660 Hertz");
//      System.err.println(new java.util.Date());
//      
//      sineWaveGenerator.play(660, 10 * 1000, volume); // play many seconds
//      
//      System.err.println(new java.util.Date());
//      
//      sineWaveGenerator.close();
//  }

  public static void main(String[] args) throws Exception {
      SineWaveGenerator sineWaveGenerator = new SineWaveGenerator();
      
      int volume = SineWaveGenerator.MAXIMUM_AMPLITUDE / 3;
      int duration = 500;

      System.err.println("Playing 440 Hertz");
      long time = System.currentTimeMillis();
      sineWaveGenerator.play(440, duration, volume);
      System.err.println("Ended playing "+duration+" millis after "+(System.currentTimeMillis() - time)+" millis");
      
      System.err.println("Playing 660 Hertz");
      time = System.currentTimeMillis();
      sineWaveGenerator.play(660, duration / 2, volume);
      System.err.println("Ended playing "+(duration / 2)+" millis after "+(System.currentTimeMillis() - time)+" millis");
      
      System.err.println("Playing 880 Hertz");
      time = System.currentTimeMillis();
      sineWaveGenerator.play(880, duration, volume);
      System.err.println("Ended playing "+duration+" millis after "+(System.currentTimeMillis() - time)+" millis");
      
      System.err.println("Playing 660 Hertz");
      time = System.currentTimeMillis();
      sineWaveGenerator.play(660, 2 * duration, volume);
      System.err.println("Ended playing "+(2 * duration)+" millis after "+(System.currentTimeMillis() - time)+" millis");
      
      System.err.println("Playing 880 Hertz");
      time = System.currentTimeMillis();
      sineWaveGenerator.play(880, duration, volume);
      System.err.println("Ended playing "+duration+" millis after "+(System.currentTimeMillis() - time)+" millis");
      
      System.err.println("Playing 1760 Hertz for 20 milliseconds");
      time = System.currentTimeMillis();
      sineWaveGenerator.play(1760, 20, volume);
      System.err.println("Ended playing "+20+" millis after "+(System.currentTimeMillis() - time)+" millis");

//      final int repeats = 1000 / SineWaveGenerator.MINIMAL_DURATION;
//      System.err.println("Playing 880 Hertz for "+SineWaveGenerator.MINIMAL_DURATION+" milliseconds "+repeats+" times ...");
//      long time = System.currentTimeMillis();
//      for (int i = 0; i < repeats; i++)
//          sineWaveGenerator.play(880, SineWaveGenerator.MINIMAL_DURATION, volume);
//      System.err.println("... this took "+((System.currentTimeMillis() - time) / (double) 1000)+" seconds");
      
      System.err.println("Starting to play A and C# simultaneously");

      SineWaveGenerator sineWaveGenerator2 = new SineWaveGenerator();
      time = System.currentTimeMillis();
      sineWaveGenerator.start(440, 50);
      sineWaveGenerator2.start(554 /*523*/, 50);
      Thread.sleep(2000);
      sineWaveGenerator.stop();
      sineWaveGenerator2.stop();
      System.err.println("Ended playing "+2000+" millis after "+(System.currentTimeMillis() - time)+" millis");
      
      System.err.println("Finished to play A and C# as chord");
      sineWaveGenerator2.close();        
      
      sineWaveGenerator.close();
  }

//  public static void main(String[] args) throws Exception {
//      SineWaveGenerator sineWaveGenerator = new SineWaveGenerator();
//      
//      System.out.println(new java.util.Date());
//
//      sineWaveGenerator.start(660, 50);
//      Thread.sleep(1000);
//      sineWaveGenerator.stop();
//      
//      System.out.println(new java.util.Date());
//      Thread.sleep(400);
//      
//      sineWaveGenerator.start(660, 50);
//      Thread.sleep(1000);
//      sineWaveGenerator.stop();
//      
//      System.out.println(new java.util.Date());
//      
//      sineWaveGenerator.close();
//  }

}
