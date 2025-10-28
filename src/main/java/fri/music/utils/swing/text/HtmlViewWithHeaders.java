package fri.music.utils.swing.text;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;

/**
 * Provides extracted header information about HTML heading elements h1-h6 and 
 * all elements with an id. Ignores any heading if it contains an anchor-element
 * with <code>href</code> attribute (like it is in a hyperlinking table-of-content).
 * Mind that this class makes no sense without a <code>HeaderListener</code> instance!
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
            sb.append(textContent);
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
    
    private int currentChapterLevel = 1;
    
    /**
     * @param url optional, HTML location to render.
     * @param headerListener required, consumer of heading list h1-h6.
     */
    public HtmlViewWithHeaders(URL url, HeaderListener headerListener) {
        super(url);
        this.headerListener = Objects.requireNonNull(headerListener);
    }
    
    @Override
    protected void startLoadingPage() {
        headerListener.startLoadingPage();
    }
    
    @Override
    protected void endLoadingPage() {
        try {
            final List<HeaderElement> headers = getTableOfContents(getHtmlDocument());
            headerListener.endLoadingPage(headers);
        }
        catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    

    private List<HeaderElement> getTableOfContents(HTMLDocument document) throws BadLocationException, IOException {
        currentChapterLevel = 1;
        
        final List<HeaderElement> headers = new ArrayList<>();
        scanHeaders(document, document.getDefaultRootElement(), headers);
        
        return headers;
    }
    
    private void scanHeaders(HTMLDocument document, Element element, List<HeaderElement> headers) throws BadLocationException {
        if (element.isLeaf())
            return; // BranchElement is where the HTML tags are
        
        final boolean isChapterHeading = isChapterHeading(element);
        final String id = getId(element);
        
        if (isChapterHeading || id != null) { // we want H1-H6 and any element with an id, like <li>
            final int level; // will be indentation of HeaderElements
            if (isChapterHeading) {
                currentChapterLevel = Integer.valueOf(element.getName().substring(1)); // get the 1 from "H1"
                level = currentChapterLevel;
            }
            else { // being below some heading at an element with ID
                level = currentChapterLevel + 1;
            }
            
            final boolean elementWithId = (isChapterHeading == false);
            if (elementWithId || containsHyperlink(element) == false) { // ignore table-of-content headings containing a hyperlink
                final String textContent = getText(document, element);
                if (textContent.length() > 0) { // ignore elements that have no text to display in "Chapters" overview
                    final HeaderElement header = new HeaderElement(level, id, textContent, element.getStartOffset());
                    headers.add(header);
                }
            }
        }
        
        for (int i = 0; i < element.getElementCount(); i++)
            scanHeaders(document, element.getElement(i), headers);
    }
    
    private String getId(Element element) throws BadLocationException {
        return getAttribute(element, HTML.Attribute.ID);
    }

    private String getAttribute(Element element, HTML.Attribute attribute) throws BadLocationException {
        if (element.getAttributes().isDefined(attribute) == false)
            return  null; // fix: getAttribute() would ask parent for inherited value
        final Object value = element.getAttributes().getAttribute(attribute);
        return (value == null) ? null : value.toString();
    }

    private String getHyperlink(Element element) throws BadLocationException {
        final String hyperlink = getTag(element, HTML.Tag.A);
        if (hyperlink == null)
            return null;
        return hyperlink.substring(hyperlink.indexOf('=') + 1); // get "#apps" from "href=#apps"
    }

    private String getTag(Element element, HTML.Tag tag) throws BadLocationException {
        if (element.getAttributes().isDefined(tag) == false)
            return  null; // fix: getAttribute() would ask parent for inherited value
        final Object value = element.getAttributes().getAttribute(tag);
        return (value == null) ? null : value.toString();
    }

    private boolean containsHyperlink(Element element) throws BadLocationException {
        for (int i = 0; i < element.getElementCount(); i++) {
            final Element subElement = element.getElement(i);
            final String hyperlink = getHyperlink(subElement);
            if (hyperlink != null && hyperlink.length() > 0)
                return true;
        }
        return false;
    }

    private boolean isChapterHeading(Element element) {
      final String name = element.getName();
      return
          name.equals(HTML.Tag.H1.toString()) ||
          name.equals(HTML.Tag.H2.toString()) ||
          name.equals(HTML.Tag.H3.toString()) ||
          name.equals(HTML.Tag.H4.toString()) ||
          name.equals(HTML.Tag.H5.toString()) ||
          name.equals(HTML.Tag.H6.toString());
    }

    /** @return just the first non-empty text of given element. */
    private String getText(HTMLDocument document, Element element) throws BadLocationException {
        final StringBuilder all = new StringBuilder();
        for (int i = 0; all.length() <= 0 && i < element.getElementCount(); i++) {
            final Element child = element.getElement(i);
            final int start = child.getStartOffset();
            final int end = child.getEndOffset();
            if (start < end) {
                String content = document.getText(start, end - start);
                all.append(content);
            }
        }
        return all.toString().trim();
    }
}