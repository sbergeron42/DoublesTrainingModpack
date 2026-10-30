//! Resource-family membership for the verified 13.0.4 selection pipeline.
//!
//! Native Trainer selectors 36/37/38 create all three Pokemon. The historical
//! PTRAINER selector 114 also denotes that family. Standalone selectors
//! 111/112/113 create only themselves and must not expand to Trainer resources.

pub fn selected_kind_includes(selected: i32, requested: i32) -> bool {
    if selected < 0 || requested < 0 {
        return false;
    }
    selected == requested
        || (matches!(selected, 36 | 37 | 38 | 114) && matches!(requested, 36 | 37 | 38))
}

#[cfg(test)]
mod tests {
    use super::selected_kind_includes;

    #[test]
    fn every_trainer_starter_includes_the_complete_family() {
        for selected in [36, 37, 38, 114] {
            for requested in [36, 37, 38] {
                assert!(selected_kind_includes(selected, requested));
            }
            assert!(!selected_kind_includes(selected, 0));
            for standalone in [111, 112, 113] {
                assert!(!selected_kind_includes(selected, standalone));
            }
        }
    }

    #[test]
    fn standalone_and_ordinary_selections_do_not_expand() {
        for selected in [0, 56, 67, 111, 112, 113] {
            assert!(selected_kind_includes(selected, selected));
            for requested in [36, 37, 38, 114] {
                assert!(!selected_kind_includes(selected, requested));
            }
        }
        assert!(!selected_kind_includes(111, 112));
    }

    #[test]
    fn unset_kinds_never_match() {
        for selected in [-1, 0, 36, 114] {
            assert!(!selected_kind_includes(selected, -1));
        }
        assert!(!selected_kind_includes(-1, 36));
    }
}
