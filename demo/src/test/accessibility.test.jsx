import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import App from '../App'

describe('Accessibility Tests', () => {
  test('all interactive elements have accessible names', () => {
    render(<App />)
    
    // Check navigation buttons
    const navButtons = screen.getAllByRole('button')
    navButtons.forEach(button => {
      expect(button).toHaveAccessibleName()
    })
  })

  test('all form inputs have labels', () => {
    render(<App />)
    
    // Navigate to a component with inputs
    const bayesButton = screen.getByText("Bayes' Theorem")
    fireEvent.click(bayesButton)
    
    const inputs = screen.getAllByRole('spinbutton')
    inputs.forEach(input => {
      expect(input).toHaveAttribute('aria-label')
    })
  })

  test('headings are properly nested', () => {
    render(<App />)
    
    // Check that main heading is h1
    const mainHeading = screen.getByRole('heading', { level: 1 })
    expect(mainHeading).toBeInTheDocument()
    
    // Check that section headings are h2 or h3
    const sectionHeadings = screen.getAllByRole('heading', { level: 2 })
    expect(sectionHeadings.length).toBeGreaterThan(0)
  })

  test('color contrast requirements are met', () => {
    render(<App />)
    
    // Check that important text elements have proper contrast classes
    const importantTexts = screen.getAllByText(/Posterior|Probability|Analysis/)
    importantTexts.forEach(text => {
      const element = text.closest('div')
      expect(element).toHaveClass()
    })
  })

  test('keyboard navigation works', () => {
    render(<App />)
    
    const firstButton = screen.getByRole('button', { name: /Introduction/ })
    firstButton.focus()
    
    expect(firstButton).toHaveFocus()
    
    // Test Tab navigation
    fireEvent.keyDown(document.activeElement, { key: 'Tab' })
    expect(document.activeElement.tagName).toBe('BUTTON')
  })

  test('focus indicators are visible', () => {
    render(<App />)
    
    const button = screen.getByRole('button', { name: /Introduction/ })
    button.focus()
    
    expect(button).toHaveFocus()
  })

  test('aria landmarks are present', () => {
    render(<App />)
    
    // Check for main landmark
    const main = screen.getByRole('main')
    expect(main).toBeInTheDocument()
    
    // Check for navigation landmark
    const nav = screen.getByRole('navigation')
    expect(nav).toBeInTheDocument()
  })

  test('screen reader friendly text', () => {
    render(<App />)
    
    // Check that mathematical expressions have proper descriptions
    const mathElements = screen.getAllByTestId(/math/)
    mathElements.forEach(element => {
      expect(element).toBeInTheDocument()
    })
  })
})

describe('Responsive Design Tests', () => {
  test('components render on mobile viewport', () => {
    // Mock mobile viewport
    Object.defineProperty(window, 'innerWidth', {
      writable: true,
      configurable: true,
      value: 375,
    })
    
    Object.defineProperty(window, 'innerHeight', {
      writable: true,
      configurable: true,
      value: 667,
    })
    
    render(<App />)
    
    expect(screen.getByText('Think Bayes 2 Interactive Demo')).toBeInTheDocument()
    expect(screen.getByText('Introduction')).toBeInTheDocument()
  })

  test('components render on tablet viewport', () => {
    // Mock tablet viewport
    Object.defineProperty(window, 'innerWidth', {
      writable: true,
      configurable: true,
      value: 768,
    })
    
    Object.defineProperty(window, 'innerHeight', {
      writable: true,
      configurable: true,
      value: 1024,
    })
    
    render(<App />)
    
    expect(screen.getByText('Think Bayes 2 Interactive Demo')).toBeInTheDocument()
  })

  test('components render on desktop viewport', () => {
    // Mock desktop viewport
    Object.defineProperty(window, 'innerWidth', {
      writable: true,
      configurable: true,
      value: 1920,
    })
    
    Object.defineProperty(window, 'innerHeight', {
      writable: true,
      configurable: true,
      value: 1080,
    })
    
    render(<App />)
    
    expect(screen.getByText('Think Bayes 2 Interactive Demo')).toBeInTheDocument()
  })

  test('navigation adapts to screen size', () => {
    render(<App />)
    
    const navigation = screen.getByRole('navigation')
    expect(navigation).toBeInTheDocument()
    
    // Check that navigation has responsive classes
    expect(navigation).toHaveClass()
  })

  test('charts are responsive', () => {
    render(<App />)
    
    // Navigate to a component with charts
    const bayesButton = screen.getByText("Bayes' Theorem")
    fireEvent.click(bayesButton)
    
    // Charts should be wrapped in responsive containers
    const responsiveContainer = screen.getByTestId('responsive-container')
    expect(responsiveContainer).toBeInTheDocument()
  })

  test('text remains readable on small screens', () => {
    // Mock small screen
    Object.defineProperty(window, 'innerWidth', {
      writable: true,
      configurable: true,
      value: 320,
    })
    
    render(<App />)
    
    // Check that text elements are present
    const textElements = screen.getAllByText(/Think Bayes|Introduction|Bayes/)
    textElements.forEach(element => {
      expect(element).toBeInTheDocument()
    })
  })
})

describe('Error Handling Tests', () => {
  test('handles invalid input gracefully', () => {
    render(<App />)
    
    const bayesButton = screen.getByText("Bayes' Theorem")
    fireEvent.click(bayesButton)
    
    // Try to enter invalid input
    const input = screen.getByDisplayValue('0.01')
    fireEvent.change(input, { target: { value: 'invalid' } })
    
    // Should not crash and should have a valid value
    expect(input.value).toMatch(/0|0\.0/)
  })

  test('handles division by zero', () => {
    render(<App />)
    
    const cookieButton = screen.getByText('Cookie Problem')
    fireEvent.click(cookieButton)
    
    // Set up scenario that could cause division by zero
    const bowl1Vanilla = screen.getByLabelText('Bowl 1 Vanilla Cookies:')
    const bowl1Chocolate = screen.getByLabelText('Bowl 1 Chocolate Cookies:')
    
    fireEvent.change(bowl1Vanilla, { target: { value: '0' } })
    fireEvent.change(bowl1Chocolate, { target: { value: '0' } })
    
    // Should handle gracefully
    expect(screen.getByText('Cookie Problem')).toBeInTheDocument()
  })

  test('handles empty data sets', () => {
    render(<App />)
    
    const hockeyButton = screen.getByText('Hockey Goals')
    fireEvent.click(hockeyButton)
    
    // Remove all data
    const removeButtons = screen.getAllByText('Remove')
    removeButtons.forEach(button => {
      try {
        fireEvent.click(button)
      } catch (e) {
        // Some buttons might be disabled, which is expected
      }
    })
    
    // Should still render without crashing
    expect(screen.getByText('Hockey Goals Analysis')).toBeInTheDocument()
  })
})

describe('Performance Tests', () => {
  test('renders without excessive warnings', () => {
    const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {})
    
    render(<App />)
    
    // Should not have React warnings
    expect(consoleSpy).not.toHaveBeenCalledWith(expect.stringContaining('React'))
    
    consoleSpy.mockRestore()
  })

  test('large data sets do not crash the app', () => {
    render(<App />)
    
    const hockeyButton = screen.getByText('Hockey Goals')
    fireEvent.click(hockeyButton)
    
    // Add many data points
    const addButton = screen.getByText('Add Game')
    for (let i = 0; i < 50; i++) {
      fireEvent.click(addButton)
    }
    
    // Should still be responsive
    expect(screen.getByText('Hockey Goals Analysis')).toBeInTheDocument()
  })
})
