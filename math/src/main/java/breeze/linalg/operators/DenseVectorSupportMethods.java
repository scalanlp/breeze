package breeze.linalg.operators;

/**
 * Created by dlwh on 8/14/15.
 *
 * Created because I need fallthrough
 */
final public class DenseVectorSupportMethods {
    public static double smallDotProduct_Double(double[] a, double[] b, int length) {
        double sum = 0.0;
        switch (length) {
//            case 8:
//                sum += a[7] * b[7];
//            case 7:
//                sum += a[6] * b[6];
            case 6:
                sum += a[5] * b[5];
            case 5:
                sum += a[4] * b[4];
            case 4:
                sum += a[3] * b[3];
            case 3:
                sum += a[2] * b[2];
            case 2:
                sum += a[1] * b[1];
            case 1:
                sum += a[0] * b[0];
            case 0:
                return sum;
            default:
                for (int i = 0; i < length; i++) {
                    sum += a[i] * b[i];
                }

                return sum;
        }
    }
}
