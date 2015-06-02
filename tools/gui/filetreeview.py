from __future__ import print_function
import Tkinter
from Tkinter import _flatten, _join, _stringify, _splitdict
import ttk
import os.path, platform

def winpath_to_id(p):
    (dr, pt) = os.path.splitdrive(os.path.abspath(p))
    return '/' + dr[0] + pt.replace('\\', '/')

def id_to_winpath(id):
    ps = id.split('/')
    return ps[1] + ':\\' + '\\'.join(ps[2:])

def tlmap(f, seq):
    res = map(f, seq)
    if isinstance(seq, tuple):
        return tuple(res)
    else:
        return res


class FileTreeview(ttk.Widget, Tkinter.XView, Tkinter.YView):
    """Ttk Treeview widget displays a hierarchical collection of items.

    Each item has a textual label, an optional image, and an optional list
    of data values. The data values are displayed in successive columns
    after the tree label.

    This is a version of TreeView specialised for displaying
    filesystem hierarchies with code to handle the problems caused by
    Windows paths containing backslashes."""

    def __init__(self, master=None, **kw):
        """Construct a Ttk Treeview with parent master.

        STANDARD OPTIONS

            class, cursor, style, takefocus, xscrollcommand,
            yscrollcommand

        WIDGET-SPECIFIC OPTIONS

            columns, displaycolumns, height, padding, selectmode, show

        ITEM OPTIONS

            text, image, values, open, tags

        TAG OPTIONS

            foreground, background, font, image
        """
        self.windows = platform.system() == 'Windows'
        ttk.Widget.__init__(self, master, "ttk::treeview", kw)


    def p2id(self, p):
        """Convert a path to an ID for use in the tree.  On Linux, this is a
           no-op.  On Windows, normalise the path to remove
           backslashes."""
        # Need to deal with None here...
        if p and self.windows:
            return winpath_to_id(p)
        else:
            return p

    def ps2ids(self, ps):
        """Convert path to IDs for use in the tree.  On Linux, this is a
           no-op.  On Windows, normalise paths to remove
           backslashes."""
        if not ps: return ps
        if not isinstance(ps, str):
            return tlmap(self.p2id, ps)
        else:
            return self.p2id(ps)

    def id2p(self, id):
        """Convert a tree item ID to a path.  On Linux, this is a no-op.  On
           Windows, reconstitute the path from the "de-backslashified"
           ID."""
        if id and self.windows:
            return id_to_winpath(id)
        else:
            return id

    def ids2ps(self, ids):
        """Convert tree item IDs to paths.  On Linux, this is a no-op.  On
           Windows, reconstitute the paths from the
           "de-backslashified" ID."""
        if not ids: return ids
        if not isinstance(ids, str):
            return tlmap(self.id2p, ids)
        else:
            return self.id2p(id)


    def bbox(self, item, column=None):
        """Returns the bounding box (relative to the treeview widget's
        window) of the specified item in the form x y width height.

        If column is specified, returns the bounding box of that cell.
        If the item is not visible (i.e., if it is a descendant of a
        closed item or is scrolled offscreen), returns an empty string."""
        return self._getints(self.tk.call(self._w, "bbox",
                                          self.p2id(item), column)) or ''


    def get_children(self, item=None):
        """Returns a tuple of children belonging to item.

        If item is not specified, returns root children."""
        return self.ids2ps(self.tk.splitlist(
            self.tk.call(self._w, "children", self.p2id(item) or '') or ()))


    def set_children(self, item, *newchildren):
        """Replaces item's child with newchildren.

        Children present in item that are not present in newchildren
        are detached from tree. No items in newchildren may be an
        ancestor of item."""
        self.tk.call(self._w, "children", self.p2id(item),
                     self.ps2ids(newchildren))


    def column(self, column, option=None, **kw):
        """Query or modify the options for the specified column.

        If kw is not given, returns a dict of the column option values. If
        option is specified then the value for that option is returned.
        Otherwise, sets the options to the corresponding values."""
        if option is not None:
            kw[option] = None
        return ttk._val_or_dict(self.tk, kw, self._w, "column", column)


    def delete(self, *items):
        """Delete all specified items and all their descendants. The root
        item may not be deleted."""
        self.tk.call(self._w, "delete", self.ps2ids(items))


    def detach(self, *items):
        """Unlinks all of the specified items from the tree.

        The items and all of their descendants are still present, and may
        be reinserted at another point in the tree, but will not be
        displayed. The root item may not be detached."""
        self.tk.call(self._w, "detach", self.ps2ids(items))


    def exists(self, item):
        """Returns True if the specified item is present in the tree,
        False otherwise."""
        return bool(self.tk.getboolean(self.tk.call(self._w, "exists",
                                                    self.p2id(item))))


    def focus(self, item=None):
        """If item is specified, sets the focus item to item. Otherwise,
        returns the current focus item, or '' if there is none."""
        return self.id2p(self.tk.call(self._w, "focus",
                                      self.p2id(item)))


    def heading(self, column, option=None, **kw):
        """Query or modify the heading options for the specified column.

        If kw is not given, returns a dict of the heading option values. If
        option is specified then the value for that option is returned.
        Otherwise, sets the options to the corresponding values.

        Valid options/values are:
            text: text
                The text to display in the column heading
            image: image_name
                Specifies an image to display to the right of the column
                heading
            anchor: anchor
                Specifies how the heading text should be aligned. One of
                the standard Tk anchor values
            command: callback
                A callback to be invoked when the heading label is
                pressed.

        To configure the tree column heading, call this with column = "#0" """
        cmd = kw.get('command')
        if cmd and not isinstance(cmd, basestring):
            # callback not registered yet, do it now
            kw['command'] = self.master.register(cmd, self._substitute)

        if option is not None:
            kw[option] = None

        return ttk._val_or_dict(self.tk, kw, self._w, 'heading', column)


    def identify(self, component, x, y):
        """Returns a description of the specified component under the
        point given by x and y, or the empty string if no such component
        is present at that position."""
        return self.tk.call(self._w, "identify", component, x, y)


    def identify_row(self, y):
        """Returns the item ID of the item at position y."""
        return self.id2p(self.identify("row", 0, y))


    def identify_column(self, x):
        """Returns the data column identifier of the cell at position x.

        The tree column has ID #0."""
        return self.identify("column", x, 0)


    def identify_region(self, x, y):
        """Returns one of:

        heading: Tree heading area.
        separator: Space between two columns headings;
        tree: The tree area.
        cell: A data cell.

        * Availability: Tk 8.6"""
        return self.identify("region", x, y)


    def identify_element(self, x, y):
        """Returns the element at position x, y.

        * Availability: Tk 8.6"""
        return self.identify("element", x, y)


    def index(self, item):
        """Returns the integer index of item within its parent's list
        of children."""
        return self.tk.getint(self.tk.call(self._w, "index", self.p2id(item)))


    def insert(self, parent, index, iid=None, **kw):
        """Creates a new item and return the item identifier of the newly
        created item.

        parent is the item ID of the parent item, or the empty string
        to create a new top-level item. index is an integer, or the value
        end, specifying where in the list of parent's children to insert
        the new item. If index is less than or equal to zero, the new node
        is inserted at the beginning, if index is greater than or equal to
        the current number of children, it is inserted at the end. If iid
        is specified, it is used as the item identifier, iid must not
        already exist in the tree. Otherwise, a new unique identifier
        is generated."""
        opts = ttk._format_optdict(kw)
        if iid:
            res = self.tk.call(self._w, "insert", self.p2id(parent), index,
                "-id", self.p2id(iid), *opts)
        else:
            res = self.tk.call(self._w, "insert", self.p2id(parent),
                               index, *opts)

        return self.id2p(res)


    def item(self, item, option=None, **kw):
        """Query or modify the options for the specified item.

        If no options are given, a dict with options/values for the item
        is returned. If option is specified then the value for that option
        is returned. Otherwise, sets the options to the corresponding
        values as given by kw."""
        if option is not None:
            kw[option] = None
        return ttk._val_or_dict(self.tk, kw, self._w,
                                "item", self.p2id(item))


    def move(self, item, parent, index):
        """Moves item to position index in parent's list of children.

        It is illegal to move an item under one of its descendants. If
        index is less than or equal to zero, item is moved to the
        beginning, if greater than or equal to the number of children,
        it is moved to the end. If item was detached it is reattached."""
        self.tk.call(self._w, "move", self.p2id(item), self.p2id(parent), index)

    reattach = move # A sensible method name for reattaching detached items


    def next(self, item):
        """Returns the identifier of item's next sibling, or '' if item
        is the last child of its parent."""
        return self.id2p(self.tk.call(self._w, "next", self.p2id(item)))


    def parent(self, item):
        """Returns the ID of the parent of item, or '' if item is at the
        top level of the hierarchy."""
        return self.id2p(self.tk.call(self._w, "parent", self.p2id(item)))


    def prev(self, item):
        """Returns the identifier of item's previous sibling, or '' if
        item is the first child of its parent."""
        return self.id2p(self.tk.call(self._w, "prev", self.p2id(item)))


    def see(self, item):
        """Ensure that item is visible.

        Sets all of item's ancestors open option to True, and scrolls
        the widget if necessary so that item is within the visible
        portion of the tree."""
        self.tk.call(self._w, "see", self.p2id(item))


    def selection(self, selop=None, items=None):
        """If selop is not specified, returns selected items."""
        return self.ids2ps(self.tk.call(self._w, "selection", selop,
                                        self.ps2ids(items)))


    def selection_set(self, items):
        """items becomes the new selection."""
        self.selection("set", items)


    def selection_add(self, items):
        """Add items to the selection."""
        self.selection("add", items)


    def selection_remove(self, items):
        """Remove items from the selection."""
        self.selection("remove", items)


    def selection_toggle(self, items):
        """Toggle the selection state of each item in items."""
        self.selection("toggle", items)


    def set(self, item, column=None, value=None):
        """Query or set the value of given item.

        With one argument, return a dictionary of column/value pairs
        for the specified item. With two arguments, return the current
        value of the specified column. With three arguments, set the
        value of given column in given item to the specified value."""
        res = self.tk.call(self._w, "set", self.p2id(item), column, value)
        if column is None and value is None:
            return _splitdict(self.tk, res,
                              cut_minus=False, conv=_tclobj_to_py)
        else:
            return res


    def tag_bind(self, tagname, sequence=None, callback=None):
        """Bind a callback for the given event sequence to the tag tagname.
        When an event is delivered to an item, the callbacks for each
        of the item's tags option are called."""
        self._bind((self._w, "tag", "bind", tagname), sequence, callback, add=0)


    def tag_configure(self, tagname, option=None, **kw):
        """Query or modify the options for the specified tagname.

        If kw is not given, returns a dict of the option settings for tagname.
        If option is specified, returns the value for that option for the
        specified tagname. Otherwise, sets the options to the corresponding
        values for the given tagname."""
        if option is not None:
            kw[option] = None
        return ttk._val_or_dict(self.tk, kw, self._w,
                                "tag", "configure", tagname)


    def tag_has(self, tagname, item=None):
        """If item is specified, returns 1 or 0 depending on whether the
        specified item has the given tagname. Otherwise, returns a list of
        all items which have the specified tag.

        * Availability: Tk 8.6"""
        if item is None:
            return self.tk.splitlist(
                self.tk.call(self._w, "tag", "has", tagname))
        else:
            return self.tk.getboolean(
                self.tk.call(self._w, "tag", "has", tagname, self.p2id(item)))
