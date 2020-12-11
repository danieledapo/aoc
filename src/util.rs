use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IVec<T>(Vec<T>);

impl<T> IVec<T> {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push(&mut self, v: T) {
        self.0.push(v);
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.0.iter()
    }

    pub fn enumerated(&self) -> impl Iterator<Item = (i32, &T)> + '_ {
        (0..self.len()).zip(&self.0)
    }

    pub fn len(&self) -> i32 {
        i32::try_from(self.0.len()).unwrap()
    }

    pub fn get(&self, i: i32) -> Option<&T> {
        self.0.get(usize::try_from(i).ok()?)
    }

    pub fn get_mut(&mut self, i: i32) -> Option<&mut T> {
        self.0.get_mut(usize::try_from(i).ok()?)
    }
}

impl<T> std::ops::Index<i32> for IVec<T> {
    type Output = T;
    fn index(&self, index: i32) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T> std::ops::IndexMut<i32> for IVec<T> {
    fn index_mut(&mut self, index: i32) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}
