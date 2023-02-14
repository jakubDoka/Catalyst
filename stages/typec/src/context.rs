use diags::*;
use packaging_t::*;
use storage::*;
use typec_t::*;

pub struct TypecExternalCtx<'i, 'm> {
    pub typec: &'m mut Typec,
    pub interner: &'m mut Interner,
    pub workspace: &'m mut Workspace,
    pub resources: &'i Resources,
}
