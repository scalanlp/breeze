package breeze.linalg.operators;

/**
 * Created by dlwh on 8/14/15.
 *
 * Created because I need fallthrough
 */
final public class DenseVectorSupportMethods {

    public static final int MAX_SMALL_DOT_PRODUCT_LENGTH = 6;

    /**
     * WARNING: only returns the right answer for vectors of length MAX_SMALL_DOT_PRODUCT_LENGTH or less
     * @param a
     * @param b
     * @param length
     * @return
     */
    public static double smallDotProduct_Double(double[] a, double[] b, int length) {
        double sum = 0.0;
        switch (length) {
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
            default:
                return sum;
        }
    }

    /**
     * WARNING: only returns the right answer for vectors of length MAX_SMALL_DOT_PRODUCT_LENGTH or less
     * @param a
     * @param b
     * @param length
     * @return
     */
    public static float smallDotProduct_Float(float[] a, float[] b, int length) {
        float sum = 0.0f;
        switch (length) {
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
            default:
                return sum;
        }
    }
}
