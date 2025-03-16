package fri.music.demos;

import fri.music.MathUtils;

public class MathUtilsDemo
{ 
    public static void main(String[] args) {
        double decimalNumber;
        long[] fraction;
        
        if (args.length > 0) {
            if (args.length > 1 && args.length % 2 == 0) {
                for (int i = 0; i < args.length; i += 2) {
                    final long dividend = Long.valueOf(args[i]);
                    final long divisor = Long.valueOf(args[i + 1]);
                    decimalNumber = (double) dividend / (double) divisor;
                    System.out.println(dividend+"/"+divisor+" == "+decimalNumber);
                }
            }
            else {
                for (String arg : args) {
                    decimalNumber = Double.valueOf(arg.replace(',', '.')); // replace German Komma
                    fraction = MathUtils.toFraction(decimalNumber);
                    System.out.println(fraction[0]+"/"+fraction[1]+" == "+decimalNumber);
                }
            }
        }
        else {
            decimalNumber = 1.333333333333333;
            fraction = MathUtils.toFraction(decimalNumber);
            System.out.println(fraction[0]+"/"+fraction[1]+" == "+decimalNumber);
            
            decimalNumber = 1.666666666666666;
            fraction = MathUtils.toFraction(decimalNumber);
            System.out.println(fraction[0]+"/"+fraction[1]+" == "+decimalNumber);
            
            decimalNumber = 1.777777777777777;
            fraction = MathUtils.toFraction(decimalNumber);
            System.out.println(fraction[0]+"/"+fraction[1]+" == "+decimalNumber);
            
            decimalNumber = 0.777777777777777;
            fraction = MathUtils.toFraction(decimalNumber);
            System.out.println(fraction[0]+"/"+fraction[1]+" == "+decimalNumber);
            
            decimalNumber = 1.599999999999999;
            fraction = MathUtils.toFraction(decimalNumber); // ~ 1.6
            System.out.println(fraction[0]+"/"+fraction[1]+" == "+decimalNumber);
            
            System.out.println("============================");
            
            final double goldenRatio = (Math.sqrt(5.0) + 1.0) / 2.0;
            System.out.println("Golden Ratio: "+goldenRatio);
            decimalNumber = goldenRatio;
            fraction = MathUtils.toFraction(decimalNumber);
            System.out.println(fraction[0]+"/"+fraction[1]+" == "+decimalNumber);
            
            final double goldenRatioInversion = (1.0 / goldenRatio);
            System.out.println("Inversion:    "+goldenRatioInversion);
            decimalNumber = goldenRatioInversion;
            fraction = MathUtils.toFraction(decimalNumber);
            System.out.println(fraction[0]+"/"+fraction[1]+" == "+decimalNumber);
        }
    }
}