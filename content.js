// Enhanced content script with feature toggle support
import { ColumnManager } from './features/columns.js';
import { AdBlocker } from './features/adblock.js';
import { BookmarkUI } from './features/bookmarks.js';
import { AutoReply } from './features/autoreply.js';
import { SearchEnhancer } from './features/search.js';
import { UIManager } from './utils/ui.js';

class ContentScript {
  constructor() {
    this.features = {
      multiColumn: new ColumnManager(),
      adBlocking: new AdBlocker(),
      smartBookmarks: new BookmarkUI(),
      tweetScheduler: new AutoReply(),
      enhancedSearch: new SearchEnhancer()
    };
    this.uiManager = new UIManager();
  }

  async initialize() {
    try {
      await this.loadSettings();
      this.initializeFeatures();
      this.setupMessageListener();
      this.setupMutationObservers();
    } catch (error) {
      console.error('Initialization error:', error);
    }
  }

  async loadSettings() {
    const { settings } = await chrome.storage.local.get('settings');
    this.settings = settings || this.getDefaultSettings();
  }

  initializeFeatures() {
    if (!this.settings.enabled) return;

    Object.entries(this.features).forEach(([key, feature]) => {
      if (this.settings[key]) {
        feature.initialize();
      }
    });
  }

  setupMessageListener() {
    chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
      if (message.type === 'SETTINGS_UPDATED') {
        this.handleSettingsUpdate(message.settings);
      }
      sendResponse({ success: true });
    });
  }

  handleSettingsUpdate(newSettings) {
    const oldSettings = this.settings;
    this.settings = newSettings;

    if (!newSettings.enabled) {
      this.disableAllFeatures();
      return;
    }

    // Handle individual feature toggles
    Object.entries(this.features).forEach(([key, feature]) => {
      if (newSettings[key] && !oldSettings[key]) {
        feature.initialize();
      } else if (!newSettings[key] && oldSettings[key]) {
        feature.disable();
      }
    });
  }

  disableAllFeatures() {
    Object.values(this.features).forEach(feature => {
      feature.disable();
    });
  }

  setupMutationObservers() {
    if (!this.settings.enabled) return;

    const observer = new MutationObserver(this.handleDOMChanges.bind(this));
    observer.observe(document.body, {
      childList: true,
      subtree: true,
      attributes: false
    });
  }

  handleDOMChanges(mutations) {
    if (!this.settings.enabled) return;

    const throttledUpdate = this.throttle(() => {
      Object.entries(this.features).forEach(([key, feature]) => {
        if (this.settings[key]) {
          feature.update(mutations);
        }
      });
    }, 100);
    
    throttledUpdate();
  }

  throttle(func, limit) {
    let inThrottle;
    return function() {
      const args = arguments;
      const context = this;
      if (!inThrottle) {
        func.apply(context, args);
        inThrottle = true;
        setTimeout(() => inThrottle = false, limit);
      }
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

// Initialize content script
const xProEnhancer = new ContentScript();
xProEnhancer.initialize();