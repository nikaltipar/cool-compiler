#ifndef SEMANT_H_
#define SEMANT_H_

#include "cool-tree.h"
#include "list.h"
#include "stringtab.h"
#include "symtab.h"
#include <assert.h>
#include <iostream>

#include <map>
#include <set>
#include <stack>
#include <vector>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable* ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable
{
  private:
    mutable int semant_errors;
    void install_basic_classes();
    ostream& error_stream;

    // The below objects are created in steps.
    // This is done for simplicity during implementation.
    // Excuse the duplicity of information.
    // Sometimes it yields more readable code.
    // At least while writing it.

    // I need a map from class name to class object.
    // const Class_&?
    std::map<Symbol, Class_> class_name_to_class_map {};

    // Both kept for consistency.
    // Keep them in sync.
    std::map<Symbol, std::vector<Symbol>> class_to_children_map {};
    std::map<Symbol, Symbol> class_to_parent_map {};

    // Keep these two in sync
    std::map<Symbol, std::set<Symbol>> class_to_method_names_map {};
    std::map<std::pair<Symbol, Symbol>, method_class> class_method_pair_to_method_obj_map {};

  protected:
    void add_class(Class_ c);
    void init_object_environment(Symbol class_name, SymbolTable<Symbol, Symbol>& object_env) const;
    std::stack<Symbol> get_inheritance_stack(Symbol class_name) const;

  public:
    ClassTable(Classes);
    int errors()
    {
        return semant_errors;
    }
    ostream& semant_error() const;
    ostream& semant_error(Class_ c) const;
    ostream& semant_error(Symbol filename, tree_node* t) const;

    void check_cycles();
    void validate_basic_structure();
    void create_class_to_methods_map();

    void verify_attributes();

    void populate_expressions_types() const;

    bool conforms(Symbol left_class, Symbol right_class, Symbol current_class) const;
    Symbol lub(Symbol left_class, Symbol right_class, Symbol current_class) const;

    bool is_valid_class(Symbol class_name) const;
    Class_ get_class(Symbol class_name) const;

    bool has_method(Symbol class_name, Symbol method_name) const;
    method_class get_method(Symbol class_name, Symbol method_name) const;
};

#endif
