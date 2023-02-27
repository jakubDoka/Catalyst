#[derive(Default)]
pub(crate) struct EscapeCtx;
// use std::mem;
//
// use mir::ValueMir;
// use storage::{Frames, ShadowMap, VRef};
//
// use crate::BorrowcCtx;
//
// macro_rules! owned {
//     ($binding:pat) => {
//         EscapeValue::Owned($binding)
//             | EscapeValue::Pointer(EscapePointer {
//                 owned: $binding,
//                 ..
//             })
//     };
// }
//
// #[derive(Default)]
// pub(crate) struct EscapeCtx {
//     values: ShadowMap<ValueMir, EscapeValue>,
//     pointers: Vec<VRef<ValueMir>>,
//     pointer_frame_base: usize,
// }
//
// impl EscapeCtx {
//     pub(super) fn dec(&mut self, v: VRef<ValueMir>) -> bool {
//         match &mut self.values[v] {
//             owned!(o) => {
//                 o.ref_count -= 1;
//                 o.ref_count == 0
//             }
//             EscapeValue::Unmanaged => false,
//         }
//     }
//
//     pub(super) fn inc(&mut self, v: VRef<ValueMir>) {
//         match &mut self.values[v] {
//             owned!(o) => o.ref_count += 1,
//             EscapeValue::Unmanaged => {}
//         }
//     }
//
//     pub(super) fn start_frame(&mut self) -> EscapeFrame {
//         let base = mem::replace(&mut self.pointer_frame_base, self.pointers.len());
//         EscapeFrame { base }
//     }
//
//     pub(super) fn end_frame(&mut self, frame: EscapeFrame) {
//         self.pointers.truncate(self.pointer_frame_base);
//         self.pointer_frame_base = frame.base;
//     }
//
//     pub(super) fn reference_value(&mut self, v: VRef<ValueMir>, by: VRef<ValueMir>) {
//         self.inc(v);
//         self.pointers.push(by);
//         self.values[v] = EscapeValue::Pointer(EscapePointer {
//             value: v,
//             owned: Default::default(),
//         });
//     }
// }
//
// pub(crate) struct EscapeFrame {
//     base: usize,
// }
//
// #[derive(Clone, Copy)]
// enum EscapeValue {
//     Owned(OwnedEscapeValue),
//     Pointer(EscapePointer),
//     Unmanaged,
// }
//
// impl Default for EscapeValue {
//     fn default() -> Self {
//         Self::Owned(Default::default())
//     }
// }
//
// #[derive(Clone, Copy)]
// struct EscapePointer {
//     value: VRef<ValueMir>,
//     owned: OwnedEscapeValue,
// }
//
// #[derive(Clone, Copy)]
// struct OwnedEscapeValue {
//     ref_count: u32,
// }
//
// impl Default for OwnedEscapeValue {
//     fn default() -> Self {
//         Self { ref_count: 1 }
//     }
// }
