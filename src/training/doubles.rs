/// Doubles training support: allow CPU fighters to hit each other.
///
/// In vanilla training mode every CPU is assigned to the same collision team
/// (team 0 / "no team"), so the game's team-attack filter silently drops any
/// hit whose attacker and defender share a team number.  The human player is
/// exempt from this filter by a separate rule, which is why player→CPU hits
/// always land while CPU→CPU hits never do.
///
/// Fix: each frame we assign every CPU its own unique hit-team equal to its
/// entry ID (1, 2, 3 …).  The game's collision check then sees them as
/// distinct teams and allows the hit through.  The player stays on team 0 so
/// player↔CPU interaction is completely unchanged.
///
/// If this team-based approach proves insufficient (e.g. because a separate
/// is_operation_cpu guard exists deeper in Fighter::HandleDamage), the byte
/// needle for that function is already documented in offsets.rs as a fallback.
use smash::app::{lua_bind::*, BattleObjectModuleAccessor};
use smash::lib::lua_const::*;

pub unsafe fn set_cpu_hit_team(module_accessor: &mut BattleObjectModuleAccessor) {
    let entry_id =
        WorkModule::get_int(module_accessor, *FIGHTER_INSTANCE_WORK_ID_INT_ENTRY_ID);

    // entry_id 0 is the human player — leave it on its default team.
    // For every CPU slot (1, 2, 3) we use the slot number as the team ID so
    // that no two CPUs share the same hit-team and can therefore hit each other.
    if entry_id > 0 {
        TeamModule::set_hit_team(module_accessor, entry_id);
    }
}
