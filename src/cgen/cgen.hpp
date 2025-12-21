#include "cool-tree.hpp"
#include "emit.hpp"
#include "symtab.hpp"
#include <assert.h>
#include <stdio.h>

#include <cstddef>
#include <functional>
#include <map>
#include <set>
#include <stack>
#include <vector>

enum Basicness
{
    Basic,
    NotBasic
};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable* CgenClassTableP;

class CgenNode;
typedef CgenNode* CgenNodeP;

class CgenClassTable
{
  private:
    ostream& str;
    int stringclasstag;
    int intclasstag;
    int boolclasstag;

    int next_tag {};

    const std::set<Symbol> predefined_method_classes {};

    // In cgen.cpp, many of helper emit functions use an int to refer to labels.
    // We will use this to track already used integers for labels and increment
    // when a new label is needed.
    inline static int next_label {};

    std::vector<CgenNodeP> class_nodes {};
    std::map<Symbol, int> class_tag_map {};

    std::map<Symbol, CgenNodeP> special_classes_map {};

    // The following methods emit code for
    // constants and global declarations.

    void code_global_data();
    void code_global_text();
    void code_bools(int);
    void code_select_gc();
    void code_constants();

    // The following creates an inheritance graph from
    // a list of classes.  The graph is implemented as
    // a tree of `CgenNode', and class names are placed
    // in the base class symbol table.

    void install_basic_classes();
    void install_class(CgenNodeP nd);
    void install_classes(Classes cs);

    void add_special_class(Symbol name, CgenNodeP nd);
    CgenNodeP get_special_node_from_symbol(Symbol name) const;
    bool is_special_symbol(Symbol name) const;

    void build_inheritance_tree();
    void set_relations(CgenNodeP nd);
    void construct_attribute_tables();
    void construct_dispatch_tables();

    void apply_from_root(std::function<void(CgenNodeP)> func);
    void apply_in_tag_order(std::function<void(CgenNodeP, int, ostream&)> func);

    void code_prototype_objects();
    void code_class_name_table();
    void code_dispatch_table();
    void code_class_obj_table();
    void code_object_initializers();
    void code_class_methods();

  public:
    CgenClassTable(Classes, ostream& str);

    CgenNodeP get_normal_node_from_symbol(Symbol name) const;
    int get_tag_from_symbol(Symbol name) const;
    CgenNodeP get_node_from_tag(int tag) const;

    CgenNodeP get_node_from_symbol(Symbol name) const;

    void code();
    CgenNodeP root();
    static int get_next_label()
    {
        return next_label++;
    }
};

class CgenNode : public class__class
{

  private:
    CgenNodeP parentnd;                 // Parent of class
    std::vector<CgenNodeP> children {}; // Children of class
    Basicness basic_status;             // `Basic' if class is basic
                                        // `NotBasic' otherwise

    std::vector<std::pair<Symbol, attr_class*>> attribute_table {};
    std::map<Symbol, size_t> attribute_offset_map {};

    std::vector<std::pair<Symbol, method_class*>> dispatch_table {};
    std::map<Symbol, size_t> dispatch_offset_map {};

  public:
    CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table);

    void add_child(CgenNodeP child);
    const std::vector<CgenNodeP>& get_children() const
    {
        return children;
    }
    void set_parentnd(CgenNodeP p);
    CgenNodeP get_parentnd()
    {
        return parentnd;
    }
    int basic()
    {
        return basic_status == Basic;
    }

    void construct_attribute_table();
    std::vector<std::pair<Symbol, attr_class*>> get_attribute_table() const
    {
        return attribute_table;
    }
    std::map<Symbol, size_t> get_attribute_offset_map() const
    {
        return attribute_offset_map;
    }

    void construct_dispatch_table();
    std::vector<std::pair<Symbol, method_class*>> get_dispatch_table() const
    {
        return dispatch_table;
    }
    std::map<Symbol, size_t> get_dispatch_offset_map() const
    {
        return dispatch_offset_map;
    }

    void code_prototype_object(int tag, ostream& str);
    void code_class_name_entry(int tag, ostream& str);
    void code_dispatch_entry(int tag, ostream& str);
    void code_class_obj_entry(int tag, ostream& str);
    void code_object_initializer(int tag, ostream& str, CgenClassTableP class_table, Symbol class_name);
    void code_class_method(ostream& s, CgenClassTableP class_table, Symbol class_name);
};

class BoolConst
{
  private:
    int val;

  public:
    BoolConst(int);
    void code_def(ostream&, int boolclasstag);
    void code_ref(ostream&) const;
};

// TODO avoid SymbolTable and use a custom implementation.
// SymbolTable does not allow clearing heap easily.
class Tracker : public SymbolTable<Symbol, int>
{
  private:
    int next_parameter_label {};

    int next_local_label {};
    std::stack<int> local_labels_stack {};

  public:
    Tracker()
        : SymbolTable<Symbol, int>() {};

    void add_parameter_id(Symbol s)
    {
        next_parameter_label++;
        SymbolTable<Symbol, int>::addid(s, new int(next_parameter_label));
    }

    void add_local_id(Symbol s)
    {
        next_local_label--;
        SymbolTable<Symbol, int>::addid(s, new int(next_local_label));
    }

    void enterscope()
    {
        local_labels_stack.push(next_local_label);
        SymbolTable<Symbol, int>::enterscope();
    }

    void exitscope()
    {
        next_local_label = local_labels_stack.top();
        local_labels_stack.pop();
        SymbolTable<Symbol, int>::exitscope();
    }
};
