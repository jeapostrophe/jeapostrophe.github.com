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
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.util.Random;
import java.util.ArrayList;

class C23 {
    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    createAndShowGUI(); } }); }

    private static void createAndShowGUI() {
        JFrame f = new JFrame("Collider");
        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        f.setSize(CollidePanel.W,CollidePanel.H);
        final CollidePanel pp = new CollidePanel(f);
        f.add(pp);
        f.setVisible(true);

        int delay = 17; //milliseconds
        ActionListener taskPerformer = new ActionListener() {
                public void actionPerformed(ActionEvent evt) {
                    pp.go(); } };
        new Timer(delay, taskPerformer).start(); } }

// X vs Y: http://www.realtimerendering.com/intersections.html

// Point
// Velocity
// AABB - (center, half-w, half-h)

// Static vs Static - Hit(position, normal, delta, time)

// Moving vs Static - Sweep(Hit, pos, time)

// Brute Force
// Grid
// Fixed Quad-tree
// Adaptive Quad-tree

class CollidePanel extends JPanel {
    public Dimension getPreferredSize() {
        return new Dimension(W,H);
    }

    static int W = 640;
    static int H = 640;
    static int N = 10000;
    static double BSIDE = 1.0;

    JFrame f;
    ArrayList<Box> boxes;
    public CollidePanel(JFrame f) {
        this.f = f;
        Random r = new Random();
        this.boxes = new ArrayList<Box>();
        for (int i = 0; i < N; i++) {
            double x = r.nextDouble() * W;
            double y = r.nextDouble() * H;
            double dx = (r.nextDouble() - 0.5) * 5.0;
            double dy = (r.nextDouble() - 0.5) * 5.0;
            this.boxes.add( new Box( BSIDE, BSIDE, x, y, dx, dy ) );
        }
    }

    class Box {
        public double x, y, dx, dy, w, h, hw, hh;
        Box( double w, double h, double x, double y, double dx, double dy ) {
            this.w  =  w; this.h  =  h;
            this.x  =  x; this.y  =  y;
            this.dx = dx; this.dy = dy;

            this.hw = w / 2.0;
            this.hh = h / 2.0;
        }

        double cx() {
            return x + this.hw;
        }
        double cy() {
            return y + this.hh;
        }
        double minx() { return x; }
        double maxx() { return x + w; }
        double miny() { return y; }
        double maxy() { return y + h; }

        boolean straddle( double x1, double w1,
                                 double x2, double w2 ) {

            double min1 = x1;
            double max1 = x1 + w1;
            double min2 = x2;
            double max2 = x2 + w2;

            return min2 <= max1 && min1 <= max2;
        }

        boolean intersectsB( Box b ) {
            return straddle(this.x, this.w, b.x, b.w)
                && straddle(this.y, this.h, b.y, b.h);
        }

        int intersects( Box b ) {
            double dx = b.cx() - this.cx();
            double px = (b.hw + this.hw) - Math.abs(dx);
            if (px <= 0) { return 0; }

            double dy = b.cy() - this.cy();
            double py = (b.hh + this.hh) - Math.abs(dy);
            if (py <= 0) { return 0; }

            if ( px < py ) {
                return 2;
            } else {
                return 1;
            }

            /*
    hit = new Hit(this)
    if px < py
      sx = sign(dx)
      hit.delta.x = px * sx
      hit.normal.x = sx
      hit.pos.x = this.cx + (this.hw * sx)
      hit.pos.y = b.cy
    else
      sy = sign(dy)
      hit.delta.y = py * sy
      hit.normal.y = sy
      hit.pos.x = b.cx
      hit.pos.y = this.cy + (this.hh * sy)
    return hit
            */
        }

        void moveStupid() {
            this.x += this.dx;
            this.y += this.dy;
        }
        void move(SpatialDB sdb) {
            double ox = this.x;
            double oy = this.y;
            
            this.x += this.dx;
            this.y += this.dy;
            for (Box b : sdb.nearMe(this)) {
                if ( this == b ) continue;
                
                int which = this.intersects(b);
                if (which > 0) {
                    if (which == 1) { this.dy *= -1; }
                    if (which == 2) { this.dx *= -1; }
                    this.x = ox;
                    this.y = oy;
                    return;
                }
            }
        }
    }

    interface SpatialDB {
        ArrayList<Box> nearMe( Box me );
        void add( Box b );
    }

    class SDB_AL implements SpatialDB {
        ArrayList<Box> al;
        SDB_AL() {
            this.al = new ArrayList<Box>();
        }
        public ArrayList<Box> nearMe( Box me ) {
            return this.al;
        }
        public void add( Box b ) {
            this.al.add(b);
        }
    }

    class SDB_Grid implements SpatialDB {
        ArrayList al[][];
        int X, Y, W, H;
        SDB_Grid(int W, int H, int X, int Y) {
            this.X = X; this.W = W;
            this.Y = Y; this.H = H;
            this.al = new ArrayList[X][Y];
            for (int x = 0; x < X; x++ ) {
                for (int y = 0; y < Y; y++ ) {
                    this.al[x][y] = new ArrayList<Box>();
                }
            }
        }

        int clamp(int m, int M, int x) {
            return Math.max(m, Math.min(M, x));
        }

        int gridx ( double x ) {
            return clamp(0, this.X-1, (int)Math.round(x / this.W * this.X));
        }
        int gridy ( double y ) {
            return clamp(0, this.Y-1, (int)Math.round(y / this.H * this.Y));
        }
        
        public ArrayList<Box> nearMe( Box me ) {
            ArrayList<Box> near = new ArrayList<Box>();

            for ( int x = gridx(me.minx()); x <= gridx(me.maxx()); x++ ) {
                for ( int y = gridy(me.miny()); y <= gridy(me.maxy()); y++ ) {
                    near.addAll(this.al[x][y]);
                }
            }
            
            return near;
        }
        public void add( Box me ) {
            for ( int x = gridx(me.minx()); x <= gridx(me.maxx()); x++ ) {
                for ( int y = gridy(me.miny()); y <= gridy(me.maxy()); y++ ) {
                    this.al[x][y].add(me);
                }
            }
        }
    }

    void go() {
        this.revalidate();
        this.repaint();

        long start = System.currentTimeMillis();

        SpatialDB other_boxes;
        // other_boxes = new SDB_AL(); // 120ms
        /*        other_boxes = new SDB_Grid(W, H, 10, 10); // 5ms
        other_boxes = new SDB_Grid(W, H, 5, 5); // 16ms
        other_boxes = new SDB_Grid(W, H, 50, 50); // 2ms
        other_boxes = new SDB_Grid(W, H, W, H); // 120ms
        other_boxes = new SDB_Grid(W, H,
                                   (int)Math.ceil(W/BSIDE),
                                   (int)Math.ceil(H/BSIDE)); // 300ms
        */
        other_boxes = new SDB_Grid(W, H, 50, 50); // 2ms
        
        other_boxes.add(new Box(W, 10.0, 0.0, -10.0, 0.0, 0.0 ));
        other_boxes.add(new Box(W, 10.0, 0.0, H-10.0, 0.0, 0.0 ));
        other_boxes.add(new Box(10.0, H, -10.0, 0.0, 0.0, 0.0 ));
        other_boxes.add(new Box(10.0, H, W, 0.0, 0.0, 0.0 ));
        for (Box b : this.boxes) {
            b.move(other_boxes);
            other_boxes.add(b);
        }

        long end = System.currentTimeMillis();

        f.setTitle("Bang! " + (end-start) );
    }

    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2d = (Graphics2D) g;

        g2d.setColor(Color.BLACK);
        for (Box b : this.boxes) {
            Rectangle2D r2d = new Rectangle2D.Double(b.x, b.y, b.w, b.h);
            g2d.fill(r2d);
        }
        
        /*
        g2d.setColor(Color.RED);
        g2d.fillOval(320,320,6,6);

        Rectangle2D r2d = new Rectangle2D.Double(10.5, 10.2, 130.1, 130.6);
        g2d.draw(r2d);
        
        g2d.setColor(Color.BLACK);
        g2d.fillRect(0,0,30,30);
        */
    }
}
