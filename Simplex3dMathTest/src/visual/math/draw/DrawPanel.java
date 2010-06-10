/*
 * Simplex3d, MathTest package
 * Copyright (C) 2010, Simplex3d Team
 *
 * This file is part of Simplex3dMathTest.
 *
 * Simplex3dMathTest is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dMathTest is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package visual.math.draw;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import javax.swing.JPanel;
import javax.swing.Timer;


/**
 * @author Aleksey Nikiforov (lex)
 */
public class DrawPanel extends JPanel implements ActionListener {

    private Painter painter;
    private boolean drawFps = true;

    private Timer timer;
    private FpsTimer fpsTimer;

    private BufferedImage img;
    private int width;
    private int height;
    private String fps = "";
    private int count = 0;

    public DrawPanel() {
        timer = new Timer(1, this);
        timer.start();
        fpsTimer = new FpsTimer();
    }

    public void setPainter(Painter p) {
        painter = p;
    }

    public boolean getDrawFps() {
        return drawFps;
    }

    public void setDrawFps(boolean d) {
        drawFps = d;
    }

    @Override public void paint(Graphics g) {
        fpsTimer.update();

        if (width != getWidth() || height != getHeight()) {
            width = getWidth();
            height = getHeight();
            img = new BufferedImage(
                width,
                height,
                BufferedImage.TYPE_INT_RGB
            );
        }

        if (painter != null) {
            int[] buffer = painter.paint(width, height);
            img.setRGB(0, 0, width, height, buffer, 0, width);
            g.drawImage(img, 0, 0, null);
        }

        if (drawFps) {
            count ++;
            int interval = (int) (fpsTimer.fps()/5);
            if (interval < 1) interval = 1;
            if (count % interval == 0) {
                count = 0;
                fps = String.valueOf((int) (fpsTimer.fps()));
            }

            g.setColor(Color.LIGHT_GRAY);
            g.fillRect(9, 6, 32, 16);
            g.setColor(Color.BLACK);

            Font bold = new Font("Monospaced", Font.BOLD, 16);
            g.setFont(bold);
            g.setColor(Color.BLACK);
            g.drawString(fps, 10, 20);
        }
    }

    public void actionPerformed(ActionEvent e) {
        repaint();
    }
}
