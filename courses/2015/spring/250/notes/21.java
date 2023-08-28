import java.util.Arrays;

class Complex {
    private final double re;   // the real part
    private final double im;   // the imaginary part

    // create a new object with the given real and imaginary parts
    public Complex(double real, double imag) {
        re = real;
        im = imag;
    }

    public Complex(double real) {
        re = real;
        im = 0.0;
    }

    // return a string representation of the invoking Complex object
    public String toString() {
        if (im == 0) return re + "";
        if (re == 0) return im + "i";
        if (im <  0) return re + " - " + (-im) + "i";
        return re + " + " + im + "i";
    }

    // return abs/modulus/magnitude and angle/phase/argument
    public double abs()   { return Math.hypot(re, im); }  // Math.sqrt(re*re + im*im)
    public double phase() { return Math.atan2(im, re); }  // between -pi and pi

    // return a new Complex object whose value is (this + b)
    public Complex plus(Complex b) {
        Complex a = this;             // invoking object
        double real = a.re + b.re;
        double imag = a.im + b.im;
        return new Complex(real, imag);
    }

    // return a new Complex object whose value is (this - b)
    public Complex minus(Complex b) {
        Complex a = this;
        double real = a.re - b.re;
        double imag = a.im - b.im;
        return new Complex(real, imag);
    }

    // return a new Complex object whose value is (this * b)
    public Complex times(Complex b) {
        Complex a = this;
        double real = a.re * b.re - a.im * b.im;
        double imag = a.re * b.im + a.im * b.re;
        return new Complex(real, imag);
    }

    // scalar multiplication
    // return a new object whose value is (this * alpha)
    public Complex times(double alpha) {
        return new Complex(alpha * re, alpha * im);
    }

    // return a new Complex object whose value is the conjugate of this
    public Complex conjugate() {  return new Complex(re, -im); }

    // return a new Complex object whose value is the reciprocal of this
    public Complex reciprocal() {
        double scale = re*re + im*im;
        return new Complex(re / scale, -im / scale);
    }

    // return the real or imaginary part
    public double re() { return re; }
    public double im() { return im; }

    // return a / b
    public Complex divides(Complex b) {
        Complex a = this;
        return a.times(b.reciprocal());
    }

    // return a new Complex object whose value is the complex exponential of this
    public Complex exp() {
        return new Complex(Math.exp(re) * Math.cos(im), Math.exp(re) * Math.sin(im));
    }

    // return a new Complex object whose value is the complex sine of this
    public Complex sin() {
        return new Complex(Math.sin(re) * Math.cosh(im), Math.cos(re) * Math.sinh(im));
    }

    // return a new Complex object whose value is the complex cosine of this
    public Complex cos() {
        return new Complex(Math.cos(re) * Math.cosh(im), -Math.sin(re) * Math.sinh(im));
    }

    // return a new Complex object whose value is the complex tangent of this
    public Complex tan() {
        return sin().divides(cos());
    }



    // a static version of plus
    public static Complex plus(Complex a, Complex b) {
        double real = a.re + b.re;
        double imag = a.im + b.im;
        Complex sum = new Complex(real, imag);
        return sum;
    }



    // sample client for testing
    public static void main(String[] args) {
        Complex a = new Complex(5.0, 6.0);
        Complex b = new Complex(-3.0, 4.0);

        System.out.println("a            = " + a);
        System.out.println("b            = " + b);
        System.out.println("Re(a)        = " + a.re());
        System.out.println("Im(a)        = " + a.im());
        System.out.println("b + a        = " + b.plus(a));
        System.out.println("a - b        = " + a.minus(b));
        System.out.println("a * b        = " + a.times(b));
        System.out.println("b * a        = " + b.times(a));
        System.out.println("a / b        = " + a.divides(b));
        System.out.println("(a / b) * b  = " + a.divides(b).times(b));
        System.out.println("conj(a)      = " + a.conjugate());
        System.out.println("|a|          = " + a.abs());
        System.out.println("tan(a)       = " + a.tan());
    }

}


class C21 {

    // f(t) = cos(2 * pi * t * 0.25)
    static double f(double t) {
        return Math.cos(2 * Math.PI * t * 0.25);
    }

    static void fft_inner(double[] input,
                          Complex[] output,
                          int offset,
                          int length,
                          int stride ) {
        if ( length == 1 ) {
            output[offset] =
                new Complex(input[offset]);
        } else {
            fft_inner( input, output,
                       offset, length / 2,
                       2 * stride );
            fft_inner( input, output,
                       offset + stride,
                       length / 2,
                       2 * stride );

            System.out.println("offset = " + offset);
            System.out.println("length = " + length);
            System.out.println("stride = " + stride);
            System.out.print("output = " + Arrays.toString(output));
            
            for ( int k = 0; k < length / 2; k++ ) {
                int spot_k =
                    offset + k;
                int spot_kplus1 =
                    offset + k + length/2;
                // kth root of z^n = 1
                Complex x =
                    (new Complex(-2.0 * Math.PI * ((double) k) / ((double)length), 1.0)).exp();
                Complex mult =
                    x.times(output[spot_kplus1]);
                Complex t = output[spot_k];
                output[spot_k] = t.plus(mult);
                output[spot_kplus1] = t.minus( mult);
            }
        }
    }

    static void fft(double[] input,
                    Complex[] output) {
        fft_inner(input, output,
                  0, input.length, 1);
    }

    public static void main( String[] args ) {
        int N = 64;

        double points[] = new double[N];
        for ( int i = 0; i < N; i++ ) {
            points[i] = f((double) i);
        }

        Complex freqs[] = new Complex[N];
        for ( int i = 0; i < N; i++ ) {
            freqs[i] = new Complex(0.0, 0.0);
        }
        fft(points, freqs);

        System.out.println("math");
    }
}
