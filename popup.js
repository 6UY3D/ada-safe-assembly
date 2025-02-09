// Enhanced popup script with improved functionality
import { StorageManager } from './utils/storage.js';
import { FeatureStats } from './utils/stats.js';

class PopupManager {
  constructor() {
    this.storage = new StorageManager();
    this.stats = new FeatureStats();
    this.featureToggles = [
      'multiColumn',
      'adBlocking',
      'shadowbanChecker',
      'smartBookmarks',
      'tweetScheduler',
      'enhancedSearch'
    ];
    this.initializeUI();
    this.loadSettings();
    this.attachEventListeners();
    this.startStatsRefresh();
  }

  async initializeUI() {
    this.tabs = document.querySelectorAll('.tab-button');
    this.panels = document.querySelectorAll('.tab-panel');
    this.enableToggle = document.getElementById('extensionEnabled');
    this.toggleAllButton = document.getElementById('toggleAll');
    this.statusMessage = document.getElementById('statusMessage');
    this.syncStatus = document.getElementById('syncStatus');
    
    // Initialize feature toggles
    this.featureToggles.forEach(feature => {
      this[feature + 'Toggle'] = document.getElementById(feature);
    });

    // Initialize stats elements
    this.statsElements = {
      columnCount: document.getElementById('columnCount'),
      blockedCount: document.getElementById('blockedCount'),
      shadowbanStatus: document.getElementById('shadowbanStatus'),
      bookmarkCount: document.getElementById('bookmarkCount'),
      scheduledCount: document.getElementById('scheduledCount'),
      filterCount: document.getElementById('filterCount')
    };
  }

  async loadSettings() {
    try {
      const settings = await this.storage.get('settings') || this.getDefaultSettings();
      
      // Update main toggle
      this.enableToggle.checked = settings.enabled;
      
      // Update feature toggles
      this.featureToggles.forEach(feature => {
        this[feature + 'Toggle'].checked = settings[feature];
        this[feature + 'Toggle'].disabled = !settings.enabled;
      });

      // Update toggle all button text
      this.updateToggleAllButton(settings);
      
      this.showStatus('Settings loaded successfully', 'success');
    } catch (error) {
      console.error('Failed to load settings:', error);
      this.showStatus('Failed to load settings', 'error');
    }
  }

  attachEventListeners() {
    // Main extension toggle
    this.enableToggle.addEventListener('change', () => {
      this.updateSettings();
      this.updateFeatureTogglesState();
    });
    
    // Toggle all button
    this.toggleAllButton.addEventListener('click', () => this.handleToggleAll());
    
    // Feature toggles
    this.featureToggles.forEach(feature => {
      this[feature + 'Toggle'].addEventListener('change', () => {
        this.updateSettings();
        this.updateToggleAllButton();
      });
    });

    // Tab switching
    this.tabs.forEach(tab => {
      tab.addEventListener('click', () => this.switchTab(tab.dataset.tab));
    });

    // Bookmark search
    const searchInput = document.getElementById('bookmarkSearch');
    if (searchInput) {
      searchInput.addEventListener('input', this.debounce(() => {
        this.searchBookmarks(searchInput.value);
      }, 300));
    }

    // Scheduler form
    const schedulerForm = document.getElementById('schedulerForm');
    if (schedulerForm) {
      schedulerForm.addEventListener('submit', (e) => {
        e.preventDefault();
        this.handleScheduleTweet();
      });
    }
  }

  async handleToggleAll() {
    const allEnabled = this.featureToggles.every(feature => 
      this[feature + 'Toggle'].checked && !this[feature + 'Toggle'].disabled
    );
    
    const newState = !allEnabled;
    this.featureToggles.forEach(feature => {
      if (!this[feature + 'Toggle'].disabled) {
        this[feature + 'Toggle'].checked = newState;
      }
    });
    
    await this.updateSettings();
    this.updateToggleAllButton();
  }

  updateToggleAllButton() {
    const allEnabled = this.featureToggles.every(feature => 
      this[feature + 'Toggle'].checked && !this[feature + 'Toggle'].disabled
    );
    this.toggleAllButton.textContent = allEnabled ? 'Disable All' : 'Enable All';
  }

  updateFeatureTogglesState() {
    const enabled = this.enableToggle.checked;
    this.featureToggles.forEach(feature => {
      this[feature + 'Toggle'].disabled = !enabled;
    });
    this.toggleAllButton.disabled = !enabled;
  }

  async updateSettings() {
    try {
      const settings = {
        enabled: this.enableToggle.checked,
        ...this.featureToggles.reduce((acc, feature) => ({
          ...acc,
          [feature]: this[feature + 'Toggle'].checked
        }), {})
      };
      
      await this.storage.set('settings', settings);
      
      // Notify content script of settings change
      chrome.tabs.query({ active: true, currentWindow: true }, tabs => {
        chrome.tabs.sendMessage(tabs[0].id, { 
          type: 'SETTINGS_UPDATED', 
          settings 
        });
      });

      this.showStatus('Settings saved', 'success');
      this.updateSyncStatus();
    } catch (error) {
      console.error('Failed to save settings:', error);
      this.showStatus('Failed to save settings', 'error');
    }
  }

  switchTab(tabId) {
    this.tabs.forEach(tab => {
      tab.classList.toggle('active', tab.dataset.tab === tabId);
    });
    
    this.panels.forEach(panel => {
      panel.classList.toggle('active', panel.id === tabId);
    });

    // Load tab-specific content
    this.loadTabContent(tabId);
  }

  async loadTabContent(tabId) {
    try {
      switch (tabId) {
        case 'columns':
          await this.loadColumns();
          break;
        case 'bookmarks':
          await this.loadBookmarks();
          break;
        case 'scheduler':
          await this.loadScheduledTweets();
          break;
      }
    } catch (error) {
      console.error(`Failed to load content for tab ${tabId}:`, error);
      this.showStatus(`Failed to load ${tabId}`, 'error');
    }
  }

  async startStatsRefresh() {
    try {
      await this.refreshStats();
      setInterval(() => this.refreshStats(), 30000); // Refresh every 30 seconds
    } catch (error) {
      console.error('Failed to start stats refresh:', error);
    }
  }

  async refreshStats() {
    try {
      const stats = await this.stats.getAll();
      Object.entries(stats).forEach(([key, value]) => {
        if (this.statsElements[key]) {
          this.statsElements[key].textContent = value;
        }
      });
    } catch (error) {
      console.error('Failed to refresh stats:', error);
    }
  }

  showStatus(message, type = 'info') {
    this.statusMessage.textContent = message;
    this.statusMessage.className = `status-message ${type}`;
    setTimeout(() => {
      this.statusMessage.textContent = '';
      this.statusMessage.className = 'status-message';
    }, 3000);
  }

  updateSyncStatus() {
    const now = new Date();
    this.syncStatus.textContent = `Last synced: ${now.toLocaleTimeString()}`;
  }

  debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
      const later = () => {
        clearTimeout(timeout);
        func(...args);
      };
      clearTimeout(timeout);
      timeout = setTimeout(later, wait);
    };
  }

  getDefaultSettings() {
    return {
      enabled: true,
      multiColumn: true,
      adBlocking: true,
      shadowbanChecker: true,
      smartBookmarks: true,
      tweetScheduler: false,
      enhancedSearch: true
    };
  }
}

// Initialize popup
document.addEventListener('DOMContentLoaded', () => {
  new PopupManager();
});