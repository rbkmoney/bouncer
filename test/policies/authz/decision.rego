package service.authz.decision

import data.service.authz.api

decide(assertions) = d {
    count(assertions.forbidden) > 0
    d := {
        "resolution": ["forbidden", assertions.forbidden]
    }
}

decide(assertions) = d {
    count(assertions.forbidden) == 0
    assertions.restrictions != {}
    count(assertions.allowed) > 0
    d := {
        "resolution": ["restricted", assertions.allowed],
        "restrictions": assertions.restrictions
    }
}

decide(assertions) = d {
    count(assertions.forbidden) == 0
    assertions.restrictions == {}
    count(assertions.allowed) > 0
    d := {
        "resolution": ["allowed", assertions.allowed]
    }
}

decide(assertions) = d {
    count(assertions.forbidden) == 0
    count(assertions.allowed) == 0
    d := {
        "resolution": ["forbidden", []]
    }
}
