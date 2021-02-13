import gi
gi.require_version("Gtk", "3.0")
import statistics
import subprocess
import time
from enum import Enum
from gi.repository import GObject, Gtk
from types import SimpleNamespace

Padding = SimpleNamespace(
    vertical=8
)


class BatteryStatus(Enum):
    Charging = "Charging"
    Discharging = "Discharging"


class BatteryWidget(Gtk.Window):
    def __init__(self, update_interval=0, samples=20, sample_interval=0.05):
        Gtk.Window.__init__(self)
        self.samples = samples
        self.sample_interval = sample_interval
        # Vertical container.
        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
        self.add(vbox)
        # Batter charge bar.
        self.charge_bar = Gtk.ProgressBar()
        vbox.pack_start(self.charge_bar, False, False, Padding.vertical)
        # Time to full/empty text.
        self.time_label = Gtk.Label()
        self.time_label.set_alignment(0, 0)
        vbox.pack_start(self.time_label, False, False, Padding.vertical)
        # Power source text.
        self.power_source_label = Gtk.Label()
        self.power_source_label.set_alignment(0, 0)
        vbox.pack_start(self.power_source_label, False, False, Padding.vertical)
        # Update once, then loop.
        def update_loop():
            self.update_state()
            self.update_view()
            GObject.timeout_add(update_interval / 1000, update_loop)
        update_loop()

    def update_view(self):
        print(self.charge)
        print(self.time_text)
        print(self.power_source)
        self.charge_bar.set_fraction(self.charge)
        self.time_label.set_text(self.time_text)
        self.power_source_label.set_text(f"Power Source: {self.power_source}")

    def update_state(self):
        # Acquire N samples.
        samples = []
        for _ in range(self.samples):
            # time.sleep(self.sample_interval)
            samples.append(self.get_sample())
        # Filter only valid samples (incase status changed during readings).
        last_status = samples[-1][1]
        samples = [s for s in samples if s[1] == last_status]
        # Set state from samples.
        self.charge = samples[-1][0]  # Assumed to be a "stable" reading.
        self.power_source = (
            "Battery" if last_status == BatteryStatus.Discharging
            else "Power Adapter")
        # Calculate time to full/empty from samples.
        many_mins = [s[2] for s in samples if s[2] is not None]
        if len(many_mins) == 0:
            assert self.charge == 1  # Only time we expect no time information.
            self.time_text = "Battery is Charged"
        else:
            mean_mins = statistics.mean(many_mins)
            hours = int(mean_mins / 60)
            mins = int(mean_mins - hours * 60)
            hours_mins_str = f"{hours}hrs {mins}mins"
            append = (
                "Remaining" if last_status == BatteryStatus.Discharging
                else "Until Full")
            self.time_text = f"{hours_mins_str} {append}"

    def get_sample(self):
        # Get output from 'acpi'.
        stdout = subprocess.run(["acpi", "-b"], capture_output=True).stdout
        parts = str(stdout).split(",", 2)
        # Calculate charge.
        charge = parts[1].strip().replace("%", "").replace("\\n'", "")
        charge = int(float(charge) / 100)
        # Calculate power source.
        battery_status = parts[0].split(":", 1)[1].strip()
        battery_status = (
            BatteryStatus.Charging if battery_status == "Charging"
            else BatteryStatus.Discharging)
        # Calculate minutes to full/empty.
        try:
            time_parts = parts[2].split(":", 2)
            mins = float(time_parts[0].strip()) * 60 + float(time_parts[1])
        except:
            mins = None
        # Return all three state components.
        return charge, battery_status, mins


def run_widget(widget):
    widget.connect("destroy", Gtk.main_quit)
    widget.show_all()
    Gtk.main()


if __name__ == "__main__":
    run_widget(BatteryWidget())
