package fri.music.swingutils.layout;

import java.awt.*;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

/**
 * Supports wrapping of components inside a JScrollPane.
 * @see https://github.com/tips4java/tips4java/blob/main/source/WrapLayout.java
 */
public class FlowLayoutForScrollPane extends FlowLayout
{
    public FlowLayoutForScrollPane() {
        super();
    }
    public FlowLayoutForScrollPane(int align) {
        super(align);
    }
    public FlowLayoutForScrollPane(int align, int hgap, int vgap) {
        super(align, hgap, vgap);
    }

    @Override
    public Dimension preferredLayoutSize(Container target) {
        return layoutSize(target, true);
    }

    @Override
    public Dimension minimumLayoutSize(Container target) {
        Dimension minimum = layoutSize(target, false);
        minimum.width -= (getHgap() + 1);
        return minimum;
    }

    /**
     * Returns the minimum or preferred dimension needed to layout the target container.
     * @param target    parent Component to get layout size for
     * @param preferred true for preferred size calculation, false for minimum size
     * @return the target dimension for the given container
     */
    private Dimension layoutSize(Container target, boolean preferred) {
        synchronized (target.getTreeLock()) {
            // Each row must fit with the width allocated to the container.
            // When the container width = 0, the preferred width of the container
            // has not yet been calculated so lets ask for the maximum.
            Container container = target;
            while (container.getSize().width == 0 && container.getParent() != null)
                container = container.getParent();

            final int parentWidth = container.getSize().width;
            final int targetWidth = (parentWidth != 0) ? parentWidth : Integer.MAX_VALUE;

            final int hgap = getHgap();
            final int vgap = getVgap();
            final Insets insets = target.getInsets();
            final int horizontalInsetsAndGap = insets.left + insets.right + (hgap * 2);
            final int maxWidth = targetWidth - horizontalInsetsAndGap;

            // Fit components into the allowed width
            final Dimension dimension = new Dimension(0, 0);
            final int members = target.getComponentCount();
            int rowWidth = 0;
            int rowHeight = 0;
            for (int i = 0; i < members; i++) {
                final Component member = target.getComponent(i);

                if (member.isVisible()) {
                    final Dimension d = preferred ? member.getPreferredSize() : member.getMinimumSize();

                    if (rowWidth + d.width > maxWidth) { // Can't add the component to current row. Start a new row.
                        addRow(dimension, rowWidth, rowHeight);
                        rowWidth = 0;
                        rowHeight = 0;
                    }

                    if (rowWidth != 0) // Add a horizontal gap for all components after the first
                        rowWidth += hgap;

                    rowWidth += d.width;
                    rowHeight = Math.max(rowHeight, d.height);
                }
            }

            addRow(dimension, rowWidth, rowHeight);

            dimension.width += horizontalInsetsAndGap;
            dimension.height += insets.top + insets.bottom + vgap * 2;

            // When using a scroll pane or the DecoratedLookAndFeel we need to
            // make sure the preferred size is less than the size of the
            // target container, so shrinking the container size works
            // correctly. Removing the horizontal gap is an easy way to do this.
            final Container scrollPane = SwingUtilities.getAncestorOfClass(JScrollPane.class, target);
            if (scrollPane != null && target.isValid())
                dimension.width -= (hgap + 1);

            return dimension;
        }
    }

    private void addRow(Dimension dimension, int rowWidth, int rowHeight) {
        dimension.width = Math.max(dimension.width, rowWidth);
        if (dimension.height > 0)
            dimension.height += getVgap();
        dimension.height += rowHeight;
    }
}