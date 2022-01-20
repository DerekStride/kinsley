use std::collections::HashMap;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Scope {
    Global,
    Local,
    Builtin,
    Free,
    Function,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Symbol {
    name: String,
    pub scope: Scope,
    pub index: usize,
}

impl Symbol {
    fn new(name: String, scope: Scope, index: usize) -> Self {
        Self { name, scope, index }
    }
}

// Using RefCell and Rc in SymbolTable makes the program not thread-safe. This could be cleaned up
// to use Arc and a mutex.
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    outer: Option<Box<SymbolTable>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn enclose(outer: SymbolTable) -> Self {
        Self {
            store: HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn define(&mut self, name: String) -> &Symbol {
        let scope = if self.outer.is_none() {
            Scope::Global
        } else {
            Scope::Local
        };

        let symbol = Symbol::new(name.clone(), scope, self.store.len());
        self.store.insert(name.clone(), symbol);

        self.store.get(&name).unwrap()
    }

    pub fn resolve(&self, name: &String) -> Option<&Symbol> {
        if let Some(x) = self.store.get(name) {
            Some(x)
        } else if let Some(outer) = &self.outer {
            outer.resolve(name)
        } else {
            None
        }
    }

    pub fn outer(&mut self) -> Option<Self> {
        if let Some(outer) = self.outer.take() {
            Some(*outer)
        } else {
            None
        }
    }

    pub fn len(&self) -> u8 {
        self.store.len() as u8
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define_and_resolve() {
        let a = "a".to_string();
        let b = "b".to_string();
        let expected = HashMap::from([
            (a.clone(), Symbol::new(a.clone(), Scope::Global, 0)),
            (b.clone(), Symbol::new(b.clone(), Scope::Global, 1)),
        ]);

        let mut global = SymbolTable::new();

        assert_eq!(0, global.define(a.clone()).index);
        assert_eq!(expected[&a], *global.resolve(&a).unwrap());

        assert_eq!(1, global.define(b.clone()).index);
        assert_eq!(expected[&b], *global.resolve(&b).unwrap());
    }

    #[test]
    fn test_resolve_local() {
        let a = "a".to_string();
        let b = "b".to_string();
        let c = "c".to_string();
        let d = "d".to_string();

        let mut global = SymbolTable::new();
        global.define(a.clone());
        global.define(b.clone());

        let mut local = SymbolTable::enclose(global);
        local.define(c.clone());
        local.define(d.clone());

        let expected = vec![
            Symbol::new(a.clone(), Scope::Global, 0),
            Symbol::new(b.clone(), Scope::Global, 1),
            Symbol::new(c.clone(), Scope::Local, 0),
            Symbol::new(d.clone(), Scope::Local, 1),
        ];

        for sym in expected {
            let result = local.resolve(&sym.name).unwrap();

            assert_eq!(sym, *result);
        };
    }
}
