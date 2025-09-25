package fri.music.swingutils.text;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;

/**
 * Provides extracted header information about HTML elements H1-H5.
 */
public class HtmlViewWithHeaders extends HtmlView
{
    /** An extracted H1-H5 chapter heading. If there is no id, use startOffset for scrolling. */
    public record HeaderElement(int level, String id, String textContent, int startOffset)
    {
        /** @return what can be seen in combo-box in navigation toolbar. */
        @Override
        public final String toString() {
            final StringBuilder sb = new StringBuilder();
            for (int i = 1; i < level; i++)
                sb.append("      ");
            sb.append(textContent());
            return sb.toString();
        }
    }
    
    /** Clients interested in headers after page-load implement this interface. */
    public interface HeaderListener
    {
        /** Called when view starts to load or reload a page. */
        void startLoadingPage();
        
        /** Called when view has scanned headers on fully loaded page. */
        void endLoadingPage(List<HeaderElement> headers);
    }
    
    private final HeaderListener headerListener;
    
    public HtmlViewWithHeaders(URL url, HeaderListener headerListener) {
        super(url);
        this.headerListener = headerListener;
    }
    
    /** Overridden to catch event when page was fully loaded, scanning headers then. */
    @Override
    public void setPage(URL url) throws IOException {
        if (headerListener != null) {
            headerListener.startLoadingPage();
            
            final PropertyChangeListener loadFinishedListener = new PropertyChangeListener() {
                @Override
                public void propertyChange(PropertyChangeEvent event)   {
                    if (event.getPropertyName().equals("page")) {
                        removePropertyChangeListener(this); // stop listening
                        try {
                            final List<HeaderElement> headers = getTableOfContents((HTMLDocument) getDocument());
                            headerListener.endLoadingPage(headers);
                        }
                        catch (Exception e) {
                            throw new RuntimeException(e);
                        }
                    }
                }
            };
            addPropertyChangeListener(loadFinishedListener);
        }

        super.setPage(url);
    }


    private List<HeaderElement> getTableOfContents(HTMLDocument document) throws BadLocationException, IOException {
        final List<HeaderElement> headers = new ArrayList<>();
        scanHeaders(document, document.getDefaultRootElement(), headers);
        return headers;
    }
    
    private int currentChapterLevel = 1;
    
    private void scanHeaders(HTMLDocument document, Element element, List<HeaderElement> headers) throws BadLocationException {
        if (element.isLeaf())
            return;
        
        final boolean isChapterHeading = isChapterHeading(element);
        final String id = getId(element);
        
        if (isChapterHeading || id != null) {
            final int level;
            if (isChapterHeading) {
                currentChapterLevel = Integer.valueOf(element.getName().substring(1));
                level = currentChapterLevel;
            }
            else {
                level = currentChapterLevel + 1;
            }
            
            final String textContent = getText(document, element);
            if (textContent.length() > 0) {
                final HeaderElement header = new HeaderElement(level, id, textContent, element.getStartOffset());
                headers.add(header);
            }
        }
        
        for (int i = 0; i < element.getElementCount(); i++) {
            scanHeaders(document, element.getElement(i), headers);
        }
    }
    
    private String getId(Element element) throws BadLocationException {
        if (element.getAttributes().isDefined(HTML.Attribute.ID) == false)
            return  null; // fix: getAttribute() would ask parent for inherited value
        final Object id = element.getAttributes().getAttribute(HTML.Attribute.ID);
        return (id == null) ? null : id.toString();
    }

    private boolean isChapterHeading(Element element) {
      final String name = element.getName();
      return
          name.equals(HTML.Tag.H1.toString()) ||
          name.equals(HTML.Tag.H2.toString()) ||
          name.equals(HTML.Tag.H3.toString()) ||
          name.equals(HTML.Tag.H4.toString()) ||
          name.equals(HTML.Tag.H5.toString());
    }

    private String getText(HTMLDocument document, Element element) throws BadLocationException {
        StringBuilder all = new StringBuilder();
        for (int i = 0; all.length() <= 0 && i < element.getElementCount(); i++) { // take just first non-empty element
            Element child = element.getElement(i);
            int start = child.getStartOffset();
            int end = child.getEndOffset();
            if (start < end) {
                String content = document.getText(start, end - start);
                all.append(content);
            }
        }
        return all.toString().trim();
    }
}