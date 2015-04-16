import javax.swing.SwingUtilities;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.BorderFactory;
import javax.swing.Timer;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.util.Random;

class C19 {
    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    createAndShowGUI();
                }
            });
    }

    private static void createAndShowGUI() {
        JFrame f = new JFrame("Plot");
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.setSize(PlotPanel.W,PlotPanel.H);
        final PlotPanel pp = new PlotPanel();
        f.add(pp);
        f.setVisible(true);

        int delay = 140; //milliseconds
        ActionListener taskPerformer = new ActionListener() {
                public void actionPerformed(ActionEvent evt) {
                    pp.go();
                }
            };
        new Timer(delay, taskPerformer).start();

    }
}


class PlotPanel extends JPanel {
    static int W = 640;
    static int H = 640;

    static int N = 40;
    double points[]; // 0,1
    static double GapSize = ((float) W) / ((float) N);
    Random r;

    public PlotPanel() {
        this.points = new double[N];
        this.r = new Random();
        for (int i = 0; i<N; i++) {
            this.points[i] = r.nextDouble();
            // System.out.println("p[" + i + "] = " + this.points[i]);
        }

        for (int i = 0; i<N; i++) {
            double x = (float)i * GapSize;
            // System.out.println("f(" + x + ") = " + f(x));
        }
    }

    public double f(double x) {
        int b = before(x);
        int a = after(x);
        double p = between(b, a, x);
        // System.out.println("\tb = " + b + " a = " + a + " p = " + p);
        return lerp(this.points[b],
                    this.points[a],
                    p);
    }

    double bucket(double x) {
        return x / GapSize;
    }

    double clamp(double x) {
        return Math.max(0, Math.min(N-1, x));
    }

    int before(double x) {
        return (int) clamp(Math.floor(bucket(x)));
    }
    int after(double x) {
        return (int) clamp(Math.ceil(bucket(x)));
    }

    double between(int left, int right, double v) {
        // System.out.println("v = " + v + " left = " + left);
        return ((double) (v - left * GapSize)) / GapSize;
    }

    double lerp(double from, double to, double p) {
        return (1.0 - p) * from + p * to;
    }

    public Dimension getPreferredSize() {
        return new Dimension(W,H);
    }

    double s = ((double) W) / 2.0;
    double os = 0.0;
    int Kmax = 1000;
    int K = Kmax;
    double warp_width = 0.0;

    void go() {
        this.revalidate();
        this.repaint();
        // Run simulated annealing
        if ( this.K > 0 ) {
            double time = (float) K / (float) Kmax;
            double cur_s = this.s;
            warp_width = ((double) W) * 0.5 * time;
            double prop_s = -1.0;
            while ( prop_s < 0.0 || prop_s >= W ) {
                prop_s = cur_s + (this.r.nextDouble() - 0.5) * warp_width;
                System.out.println("prop_s = " + prop_s);
            }
            double cur_v = f(cur_s);
            double prop_v = f(prop_s);
            boolean change_p = (cur_v < prop_v) || this.r.nextDouble() < time * 0.25;
            /* System.out.println("f(" + cur_s + ") = " + cur_v + " ? " +
                               "f(" + prop_s + ") = " + prop_v + " == " +
                               better_p);
            */
            double new_s = change_p ? prop_s : cur_s;
            this.os = prop_s;
            this.s = new_s;
            this.K = this.K - 1;
        }
    }

    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        int sx = (int)Math.round(s);

        g.setColor(Color.PINK);
        g.fillRect((int) Math.round(sx - warp_width), 0, (int) Math.round(2.0 * warp_width), H);
        
        int lx = 0; int ly = 0;
        for (int i = 0; i<N; i++) {
            int x = (int)Math.round(((float) i) * GapSize);
            int y = H - (int)Math.round(this.points[i] * ((double) H));
            g.setColor(Color.RED);
            g.drawOval(x,y,6,6);
            g.setColor(Color.BLACK);
            int ax = x + 3;
            int ay = y + 3;
            g.drawLine(lx,ly,ax,ay);
            lx = ax; ly = ay;
        }

        g.setColor(Color.BLUE);
        g.drawLine(sx, 0, sx, H);

        int osx = (int)Math.round(os);
        g.setColor(Color.GREEN);
        g.drawLine(osx, 0, osx, H);

    }
}
