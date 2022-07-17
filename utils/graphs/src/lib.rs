#![feature(let_chains)]

pub mod cycle_detector;

pub use cycle_detector::{
    CycleDetector, CycleDetectorNode, ProjectedCycleDetector, ProjectedCycleDetectorNode,
};
