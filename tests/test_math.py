import math
import unittest

# Python helpers mirror the formulas used in sg-math.lsp for verification purposes.


def gear_derived(z, module, pressure_deg, bore, kerf):
    alpha = math.radians(pressure_deg or 20.0)
    pitch = 0.5 * z * module
    addendum = module
    dedendum = 1.25 * module
    pitch_r = pitch + kerf
    base_r = pitch * math.cos(alpha) + kerf
    tip_r = pitch + addendum + kerf
    root_r = max(0.01, pitch - dedendum + kerf)
    return {
        "z": z,
        "pitch_r": pitch_r,
        "base_r": base_r,
        "tip_r": tip_r,
        "root_r": root_r,
        "half_thick": math.pi / (2 * z),
        "tooth_angle": (2 * math.pi) / z,
        "addendum": addendum,
        "dedendum": dedendum,
    }


def involute_param(rb, r):
    return math.sqrt(max(0.0, (r * r) / (rb * rb) - 1.0))


def involute_point(rb, t):
    return (
        rb * (math.cos(t) + t * math.sin(t)),
        rb * (math.sin(t) - t * math.cos(t)),
    )


def involute_curve(rb, rt, steps):
    if steps <= 0 or rb <= 0 or rt <= rb:
        return []
    t_max = involute_param(rb, rt)
    dt = t_max / steps
    pts = []
    for i in range(steps + 1):
        pts.append(involute_point(rb, dt * i))
    return pts


def polar_point(radius, ang):
    return (radius * math.cos(ang), radius * math.sin(ang))


def rotate_point(pt, ang):
    x, y = pt
    return (
        x * math.cos(ang) - y * math.sin(ang),
        x * math.sin(ang) + y * math.cos(ang),
    )


def build_tooth_outline(derived):
    rb = derived["base_r"]
    rt = derived["tip_r"]
    rr = derived["root_r"]
    tooth_ang = derived["tooth_angle"]
    half_thick = derived["half_thick"]
    gap_ang = tooth_ang - 2 * half_thick
    involute = involute_curve(rb, rt, 8)
    left_root = polar_point(rr, -half_thick)
    right_root = polar_point(rr, half_thick)
    left_flank = [rotate_point(p, -half_thick) for p in involute]
    right_flank = [rotate_point(p, half_thick) for p in involute]
    outline = []
    outline.append(left_root)
    outline.extend(left_flank)
    outline.extend(reversed(right_flank))
    outline.append(right_root)
    # root arc toward next tooth
    arc_pts = []
    delta = gap_ang / 3
    for i in range(1, 4):
        arc_pts.append(polar_point(rr, half_thick + delta * i))
    outline.extend(arc_pts)
    return outline


def gear_outline(derived):
    base_outline = build_tooth_outline(derived)
    tooth_ang = derived["tooth_angle"]
    z = derived["z"]
    pts = []
    for i in range(z):
        pts.extend([rotate_point(p, i * tooth_ang) for p in base_outline])
    return pts


class GearMathTests(unittest.TestCase):
    def test_derived_radii(self):
        g = gear_derived(z=24, module=2.0, pressure_deg=20.0, bore=5.0, kerf=0.0)
        self.assertAlmostEqual(g["pitch_r"], 24.0)
        self.assertAlmostEqual(g["base_r"], 24.0 * math.cos(math.radians(20.0)))
        self.assertAlmostEqual(g["tip_r"], 26.0)
        self.assertAlmostEqual(g["root_r"], 21.5)
        self.assertAlmostEqual(g["half_thick"], math.pi / (2 * 24))

    def test_involute_radius_growth(self):
        g = gear_derived(z=16, module=1.5, pressure_deg=20.0, bore=4.0, kerf=0.0)
        pts = involute_curve(g["base_r"], g["tip_r"], 5)
        radii = [math.hypot(x, y) for x, y in pts]
        self.assertGreater(min(radii), g["base_r"] - 1e-6)
        self.assertGreater(radii[-1], radii[0])
        self.assertAlmostEqual(radii[-1], g["tip_r"], delta=1e-3)

    def test_kerf_shifts_radii_consistently(self):
        base = gear_derived(z=30, module=1.0, pressure_deg=20.0, bore=5.0, kerf=0.0)
        widened = gear_derived(z=30, module=1.0, pressure_deg=20.0, bore=5.0, kerf=0.2)
        shrunk = gear_derived(z=30, module=1.0, pressure_deg=20.0, bore=5.0, kerf=-0.1)

        for key in ["pitch_r", "base_r", "tip_r"]:
            self.assertGreater(widened[key], base[key])
            self.assertLess(shrunk[key], base[key])

        # Root radius is clamped to avoid collapsing geometry
        self.assertGreaterEqual(widened["root_r"], 0.01)
        self.assertGreaterEqual(shrunk["root_r"], 0.01)

    def test_outline_bounds(self):
        g = gear_derived(z=12, module=2.0, pressure_deg=20.0, bore=5.0, kerf=0.0)
        outline = gear_outline(g)
        xs = [p[0] for p in outline]
        ys = [p[1] for p in outline]
        max_r = max(math.hypot(x, y) for x, y in outline)
        expected_tip = g["tip_r"]
        self.assertAlmostEqual(max_r, expected_tip, delta=expected_tip * 0.05)
        self.assertLess(min(xs), 0)
        self.assertGreater(max(xs), 0)
        self.assertLess(min(ys), 0)
        self.assertGreater(max(ys), 0)

        # Length of the outline should match the per-tooth outline replicated for each tooth.
        per_tooth = len(build_tooth_outline(g))
        self.assertEqual(len(outline), per_tooth * g["z"])

    def test_outline_symmetry_per_tooth_angle(self):
        g = gear_derived(z=18, module=1.5, pressure_deg=20.0, bore=4.0, kerf=0.0)
        outline = gear_outline(g)
        per_tooth = len(build_tooth_outline(g))
        first_segment = outline[:per_tooth]
        rotated_segment = outline[per_tooth:2 * per_tooth]

        # Compare radial distances for successive teeth; angular rotation should preserve radius at matching indices.
        for p1, p2 in zip(first_segment, rotated_segment):
            r1 = math.hypot(*p1)
            r2 = math.hypot(*p2)
            self.assertAlmostEqual(r1, r2, delta=1e-6)

        # Angular separation of the first point of two adjacent teeth should approximate the tooth angle.
        ang1 = math.atan2(first_segment[0][1], first_segment[0][0])
        ang2 = math.atan2(rotated_segment[0][1], rotated_segment[0][0])
        diff = (ang2 - ang1) % (2 * math.pi)
        self.assertAlmostEqual(diff, g["tooth_angle"], delta=1e-6)


if __name__ == "__main__":
    unittest.main()
